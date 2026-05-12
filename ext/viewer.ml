(* SPDX-License-Identifier: GPL-2.0-only *)
(* epub.ml - EPUB viewer *)

open Brr
open Fut.Syntax

let js = Jstr.v
let ( let*? ) fut f =
  let* r = fut in match r with
  | Error e -> Console.(log [Jv.Error.message e]); Fut.return ()
  | Ok v -> f v

(* — DOM helpers — *)

let body () = Document.body G.document
let raf f = ignore @@ Jv.call (Jv.get Jv.global "window")
  "requestAnimationFrame" [| Jv.callback ~arity:1 f |]
let set_prop el k v =
  ignore @@ Jv.call (Jv.get (El.to_jv el) "style") "setProperty"
    [| Jv.of_string k; Jv.of_string v |]

(* — Blob URLs from zip content — *)

let blob_url data media_type =
  match Tarray.of_binary_jstr (Jstr.binary_of_octets data) with
  | Error _ -> Jstr.empty
  | Ok ta ->
    let blob = Blob.of_array_buffer
      ~init:(Blob.init ~type':(js media_type) ()) (Tarray.buffer ta) in
    Jv.call (Jv.get Jv.global "URL") "createObjectURL" [| Blob.to_jv blob |]
    |> Jv.to_jstr

let resource_cache : (string, Jstr.t) Hashtbl.t = Hashtbl.create 64

let resource_url z (r : Epub.resource) =
  match Hashtbl.find_opt resource_cache r.href with
  | Some u -> u
  | None ->
    let u = match Epub.read_raw z r.href with
      | Error _ -> Jstr.empty | Ok data -> blob_url data r.media_type in
    Hashtbl.replace resource_cache r.href u; u

(* — XHTML security policy — *)

let src_attr = function "src" | "xlink:href" | "poster" -> true | _ -> false
let strip_el = function
  | "script" | "iframe" | "object" | "embed" | "applet" | "form" | "base" -> true
  | _ -> false
let event_attr k = String.length k >= 2 && k.[0] = 'o' && k.[1] = 'n'

let escape_css_attr s =
  let buf = Buffer.create (String.length s) in
  String.iter (function '\\' -> Buffer.add_string buf "\\\\"
    | '"' -> Buffer.add_string buf "\\\"" | c -> Buffer.add_char buf c) s;
  Buffer.contents buf

let id_selector frag = Printf.sprintf "[id=\"%s\"]" (escape_css_attr frag)

(* — XHTML → DOM renderer — *)

let render_xhtml ~rewrite xhtml parent =
  let inp = Xmlm.make_input (`String (0, xhtml)) in
  let doc = El.document parent in
  let stack = ref [parent] in
  let top () = match !stack with el :: _ -> el | [] -> parent in
  let hd = ref 0 in
  let xml_attr name =
    List.find_map (fun ((_, n), v) -> if n = name then Some v else None) in
  let rec skip d = match Xmlm.input inp with
    | `El_start _ -> skip (d + 1)
    | `El_end -> if d > 1 then skip (d - 1)
    | _ -> skip d in
  let collect_text () =
    let buf = Buffer.create 256 in
    let rec go d = match Xmlm.input inp with
      | `Data s -> Buffer.add_string buf s; go d
      | `El_start _ -> go (d + 1)
      | `El_end -> if d > 0 then go (d - 1)
      | `Dtd _ -> go d
    in go 0; Buffer.contents buf in
  let rec walk () =
    if Xmlm.eoi inp then ()
    else match Xmlm.input inp with
      | `El_start ((_ns, tag), attrs) ->
        (match tag with
         | "html" | "body" -> walk ()
         | "head" -> hd := 1; walk ()
         | _ when !hd > 0 ->
           incr hd;
           (match tag with
            | "link" when xml_attr "rel" attrs = Some "stylesheet" ->
              (match xml_attr "href" attrs with
               | Some h ->
                 let v = rewrite h in
                 if v <> "" then
                   El.append_children parent
                     [El.v ~d:doc (js "link")
                        ~at:[At.v (js "rel") (js "stylesheet");
                             At.v (js "href") (js v)] []]
               | None -> ())
            | "style" ->
              decr hd;
              El.append_children parent
                [El.v ~d:doc (js "style") [El.txt ~d:doc (js (collect_text ()))]]
            | _ -> skip 1; decr hd);
           walk ()
         | _ when strip_el tag -> skip 1; walk ()
         | _ ->
           let at = List.filter_map (fun ((_, k), v) ->
             if event_attr k then None
             else if src_attr k then
               let v = rewrite v in
               if v = "" then None else Some (At.v (js k) (js v))
             else Some (At.v (js k) (js v))) attrs in
           let el = El.v ~d:doc (js tag) ~at [] in
           El.append_children (top ()) [el];
           stack := el :: !stack; walk ())
      | `El_end ->
        if !hd > 0 then decr hd
        else (match !stack with _ :: tl -> stack := tl | [] -> ());
        walk ()
      | `Data s -> El.append_children (top ()) [El.txt ~d:doc (js s)]; walk ()
      | `Dtd _ -> walk ()
  in try walk () with Xmlm.Error ((l, c), e) ->
    El.append_children parent
      [El.v ~d:doc (js "pre")
         ~at:[At.v (js "style") (js "color:#96555F;font-size:12px;opacity:0.7;padding:1em")]
         [El.txt ~d:doc (js (Printf.sprintf "XML error at %d:%d: %s" l c
                               (Xmlm.error_message e)))]]

(* — Per-tab persistence — *)

let ss_get k =
  try let ss = Jv.get Jv.global "sessionStorage" in
      let v = Jv.call ss "getItem" [| Jv.of_string k |] in
      if Jv.is_null v then None else Some (Jv.to_string v)
  with _ -> None

let ss_set k v =
  try let ss = Jv.get Jv.global "sessionStorage" in
      ignore @@ Jv.call ss "setItem" [| Jv.of_string k; Jv.of_string v |]
  with _ -> ()

let tab_key =
  match ss_get "epub-ml-tab" with
  | Some id -> "https://epub-ml.invalid/" ^ id
  | None ->
    let id = Jv.to_string (Jv.call (Jv.get Jv.global "crypto") "randomUUID" [||]) in
    ss_set "epub-ml-tab" id;
    "https://epub-ml.invalid/" ^ id

(* — Zoom — *)

let sync_height content host factor =
  let natural = Jv.to_float (Jv.get (El.to_jv content) "scrollHeight") in
  let h = Printf.sprintf "%.0fpx" (natural *. factor) in
  set_prop host "height" h

let set_transform content factor =
  set_prop content "transform" (Printf.sprintf "scale(%.4f)" factor)

let zoom_to content ~prev ~next origin_y =
  let b = El.to_jv (body ()) in
  let top = Jv.to_float (Jv.get b "scrollTop") in
  set_transform content next;
  Jv.set b "scrollTop" (Jv.of_float (top +. (origin_y +. top) *. (next /. prev -. 1.)))

(* — Viewer — *)

let rec setup_viewer (epub : Epub.t) z =
  let idx = Hashtbl.create 64 in
  List.iter (fun (r : Epub.resource) -> Hashtbl.replace idx r.href r) epub.resources;
  let rewrite base url =
    match Hashtbl.find_opt idx (Epub.resolve_href base url) with
    | Some r -> Jstr.to_string (resource_url z r) | None -> "" in
  let title = match epub.metadata.titles with t :: _ -> t | [] -> "epub.ml" in
  Jv.set (Document.to_jv G.document) "title" (Jv.of_string title);
  El.set_class (js "picker") false (body ());
  El.set_class (js "viewer") true (body ());

  (* — toolbar — *)
  let zoom = ref 1.0 in
  let clamp z = Float.max 0.25 (Float.min 10.0 z) in
  let btn ?(cls="tb") ?aria label =
    El.v (js "button") ~at:(At.class' (js cls)
      :: (match aria with Some a -> [At.v (js "aria-label") (js a)] | None -> []))
      [El.txt' label] in
  let toc_btn = btn ~cls:"tb toc-btn" ~aria:"Table of contents" "\xe2\x89\xa1" in
  let zoom_out = btn ~aria:"Zoom out" "\xe2\x88\x92" in
  let zoom_in = btn ~aria:"Zoom in" "+" in
  let zoom_select = El.v (js "select")
    ~at:[At.class' (js "tb zoom-select"); At.v (js "aria-label") (js "Zoom level")]
    (List.map (fun lvl ->
      let opt = El.v (js "option") ~at:[At.v (js "value") (js (string_of_int lvl))]
                  [El.txt' (string_of_int lvl ^ "%")] in
      if lvl = 100 then El.set_at (js "selected") (Some (js "")) opt; opt
    ) [50; 75; 100; 125; 150; 200; 300]) in
  let page_label = El.v (js "span")
    ~at:[At.class' (js "page-label"); At.v (js "role") (js "status");
         At.v (js "aria-live") (js "polite")] [] in
  let info_btn = btn ~cls:"tb info-btn" ~aria:"Book information" "\xe2\x84\xb9" in
  El.set_at (js "aria-expanded") (Some (js "false")) toc_btn;
  El.set_at (js "aria-expanded") (Some (js "false")) info_btn;
  let toolbar = El.v (js "header") ~at:[At.v (js "id") (js "toolbar")] [
    toc_btn;
    El.v (js "div") ~at:[At.class' (js "tb-group zoom-group")]
      [zoom_out; zoom_select; zoom_in];
    El.v (js "div") ~at:[At.class' (js "tb-group")] [page_label; info_btn] ] in

  (* — info panel — *)
  let meta = epub.metadata in
  let dl_items =
    let row ?(cls="") label value =
      if value = "" then [] else
      [El.v (js "dt") [El.txt' label];
       El.v (js "dd") ~at:(if cls = "" then [] else [At.class' (js cls)])
         [El.txt' value]] in
    let row_html label html =
      if html = "" then [] else
      let dd = El.v (js "dd") [] in
      Jv.set (El.to_jv dd) "innerHTML" (Jv.of_string html);
      [El.v (js "dt") [El.txt' label]; dd] in
    let rows label = function [] -> [] | vs -> row label (String.concat ", " vs) in
    List.concat [
      row ~cls:"book-title" "dc:title" title;
      rows "dc:creator" meta.creators;
      rows "dc:contributor" meta.contributors;
      rows "dc:publisher" meta.publishers;
      row "dc:date" meta.date;
      row "dc:language" (String.concat ", " meta.languages);
      rows "dc:subject" meta.subjects;
      row_html "dc:description" meta.description;
      row "dc:rights" meta.rights;
      row "dc:source" meta.source;
      row "dc:identifier" (match meta.identifiers with x :: _ -> x | [] -> "");
      row "dcterms:modified" meta.modified ] in
  let info_panel = El.v (js "div")
    ~at:[At.v (js "id") (js "info-panel"); At.v (js "role") (js "dialog");
         At.v (js "aria-label") (js "Book information")]
    [El.v (js "dl") dl_items] in
  let info_overlay = El.v (js "div") ~at:[At.v (js "id") (js "info-overlay")]
    [info_panel] in

  (* — content — *)
  let content_host = El.v (js "main") ~at:[At.v (js "id") (js "content-host")] [] in
  let shadow = Jv.call (El.to_jv content_host) "attachShadow"
    [| Jv.obj [| "mode", Jv.of_string "open" |] |] in
  let shadow_css = String.concat "\n" [
    ":host { display: block; }";
    "#content { max-width: 38em; margin: 0 auto; padding: 2em;";
    "  background-color: var(--content-bg, #FCFAF7);";
    "  transform-origin: top center; }";
    "article { margin-bottom: 4em; }";
    "article + article { padding-top: 2em; }";
    "img { max-width: 100%; height: auto; }";
    "a { color: inherit; }" ] in
  ignore @@ Jv.call shadow "appendChild"
    [| El.to_jv (El.v (js "style") [El.txt' shadow_css]) |];
  let book_lang = match meta.languages with l :: _ -> l | [] -> "en" in
  let content = El.v (js "div") ~at:[At.v (js "id") (js "content");
    At.v (js "lang") (js book_lang)] [] in
  ignore @@ Jv.call shadow "appendChild" [| El.to_jv content |];

  (* — render spine — *)
  let views = epub.spine |> List.filter_map (fun (item : Epub.spine_item) ->
    if not item.linear then None
    else
      let el = El.v (js "article") [] in
      El.append_children content [el];
      let base = Epub.dirname item.resource.href in
      (match Epub.read_spine_item z item with
       | Ok xhtml -> render_xhtml ~rewrite:(rewrite base) xhtml el
       | Error _ -> ());
      Some (item, el)) in

  let view_of_href = Hashtbl.create 16 in
  List.iter (fun (item, el) ->
    Hashtbl.replace view_of_href (item : Epub.spine_item).resource.href el) views;

  (* — TOC — *)
  let history = Window.history G.window in
  let nav = match epub.Epub.nav_href with
    | None -> { Epub.toc = []; page_list = [] }
    | Some href -> match Epub.read_raw z href with
      | Error _ -> { Epub.toc = []; page_list = [] }
      | Ok xhtml -> Epub.parse_nav (Epub.dirname href) xhtml in
  let find_target (entry : Epub.toc_entry) =
    match Hashtbl.find_opt view_of_href entry.path with
    | None -> None
    | Some el ->
      Some (match entry.fragment with
        | None -> El.to_jv el
        | Some frag ->
          let root = Jv.call (El.to_jv el) "getRootNode" [||] in
          let found = Jv.call root "querySelector" [| Jv.of_string (id_selector frag) |] in
          if Jv.is_none found then El.to_jv el else found) in
  let toc_items = nav.toc |> List.filter_map (fun entry ->
    match find_target entry with
    | None -> None
    | Some _ ->
      let li = El.v (js "li") [] in
      let a = El.v (js "a") [El.txt' entry.label] in
      ignore @@ Ev.listen Ev.click (fun _ev ->
        find_target entry |> Option.iter (fun target ->
          let b = El.to_jv (body ()) in
          let prev_scroll = Jv.get b "scrollTop" in
          ignore @@ Jv.call target "scrollIntoView" [||];
          Window.History.push_state ~state:prev_scroll history)
      ) (El.as_target a);
      El.append_children li [a]; Some (entry, li)) in
  let sidebar = El.v (js "nav") ~at:[At.v (js "id") (js "sidebar");
    At.v (js "aria-label") (js "Table of contents")]
    (if toc_items = [] then []
     else [El.v (js "ol") (List.map snd toc_items)]) in

  (* — assemble — *)
  El.set_children (body ()) [toolbar; info_overlay; sidebar; content_host];
  set_transform content 1.0;
  sync_height content content_host 1.0;
  let obs = Jv.new' (Jv.get Jv.global "ResizeObserver")
    [| Jv.callback ~arity:1 (fun _ -> sync_height content content_host !zoom) |] in
  ignore @@ Jv.call obs "observe" [| El.to_jv content |];

  (* — history — *)
  ignore @@ Ev.listen Window.History.Ev.popstate (fun ev ->
    let state = Window.History.Ev.Popstate.state (Ev.as_type ev) in
    if Jv.is_null state then show_picker ()
    else if Jv.typeof state = js "string" then
      ignore @@ try_cache ()
    else if Jv.typeof state = js "number" then
      Jv.set (El.to_jv (body ())) "scrollTop" state
  ) (Window.as_target G.window);

  (* — scroll tracking — *)
  let active_li = ref Jv.null in
  let update_toc () =
    let best = List.fold_left (fun acc (entry, li) ->
      match find_target entry with
      | None -> acc
      | Some el ->
        let y = Jv.to_float (Jv.get (Jv.call el "getBoundingClientRect" [||]) "top") in
        if y <= 30. then Some li else acc
    ) None toc_items in
    let jv = match best with Some li -> El.to_jv li | None -> Jv.null in
    if not (Jv.equal !active_li jv) then begin
      if not (Jv.is_null !active_li) then
        El.set_class (js "active") false (El.of_jv !active_li);
      (match best with
       | Some li ->
         El.set_class (js "active") true li;
         if El.class' (js "open") sidebar then
           ignore @@ Jv.call (El.to_jv li) "scrollIntoView"
             [| Jv.obj [| "block", Jv.of_string "nearest" |] |]
       | None -> ());
      active_li := jv
    end in
  let update_progress () =
    let b = El.to_jv (body ()) in
    let top = Jv.to_float (Jv.get b "scrollTop") in
    let sh = Jv.to_float (Jv.get b "scrollHeight") in
    let ch = Jv.to_float (Jv.get (Jv.get Jv.global "window") "innerHeight") in
    let pct = if sh <= ch then 100. else Float.min 100. (Float.max 0.
                (top /. (sh -. ch) *. 100.)) in
    set_prop toolbar "--progress" (Printf.sprintf "%.4f%%" pct);
    El.set_children page_label [El.txt' (Printf.sprintf "%.1f%%" pct)];
    update_toc ();
    let sb = if El.class' (js "open") sidebar then "1" else "0" in
    ss_set "epub-ml-state" (Printf.sprintf "%.1f,%.4f,%s" top !zoom sb) in
  let pending = ref false in
  ignore @@ Ev.listen Ev.scroll (fun _ev ->
    if not !pending then begin pending := true;
      raf (fun _ -> pending := false; update_progress ()) end
  ) (El.as_target (body ()));

  (* — panel toggles — *)
  let close el btn =
    if El.class' (js "open") el then begin
      El.set_class (js "open") false el;
      El.set_at (js "aria-expanded") (Some (js "false")) btn;
      ignore @@ Jv.call (El.to_jv btn) "focus" [||] end in
  let toggle el btn =
    if El.class' (js "open") el then close el btn
    else begin
      El.set_class (js "open") true el;
      El.set_at (js "aria-expanded") (Some (js "true")) btn end in
  ignore @@ Ev.listen Ev.click (fun _ev -> toggle sidebar toc_btn) (El.as_target toc_btn);
  ignore @@ Ev.listen Ev.click (fun _ev -> toggle info_overlay info_btn) (El.as_target info_btn);
  ignore @@ Ev.listen Ev.click (fun ev ->
    if El.class' (js "open") info_overlay then
      let t = Jv.get (Ev.to_jv ev) "target" in
      if not (Jv.to_bool (Jv.call (El.to_jv info_panel) "contains" [| t |]))
      && not (Jv.to_bool (Jv.call (El.to_jv info_btn) "contains" [| t |])) then
        close info_overlay info_btn
  ) (El.as_target (body ()));

  (* — zoom controls — *)
  let update_zoom () =
    let pct = int_of_float (!zoom *. 100.) in
    sync_height content content_host !zoom;
    let s = string_of_int pct ^ "%" in
    if List.mem pct [50; 75; 100; 125; 150; 200; 300] then
      Jv.set (El.to_jv zoom_select) "value" (Jv.of_string (string_of_int pct))
    else begin
      El.find_first_by_selector ~root:zoom_select (js "option[value=custom]")
      |> Option.iter El.remove;
      El.prepend_children zoom_select
        [El.v (js "option") ~at:[At.v (js "value") (js "custom");
          At.v (js "selected") (js "")] [El.txt' s]];
      Jv.set (El.to_jv zoom_select) "value" (Jv.of_string "custom")
    end in
  let do_zoom ?origin f =
    let mid = Jv.to_float (Jv.get (Jv.get Jv.global "window") "innerHeight") /. 2. in
    let prev = !zoom in
    zoom := Float.round (clamp f *. 100.) /. 100.;
    if !zoom <> prev then begin
      zoom_to content ~prev ~next:!zoom (match origin with Some y -> y | None -> mid);
      update_zoom ()
    end in
  ignore @@ Ev.listen Ev.click (fun _ev -> do_zoom (!zoom /. 1.1)) (El.as_target zoom_out);
  ignore @@ Ev.listen Ev.click (fun _ev -> do_zoom (!zoom *. 1.1)) (El.as_target zoom_in);
  ignore @@ Ev.listen Ev.change (fun _ev ->
    let v = Jv.to_string (Jv.get (El.to_jv zoom_select) "value") in
    if v <> "custom" then do_zoom (float_of_int (int_of_string v) /. 100.)
  ) (El.as_target zoom_select);
  ignore @@ Jv.call (Jv.get Jv.global "window") "addEventListener"
    [| Jv.of_string "wheel";
       Jv.callback ~arity:1 (fun raw ->
         if Jv.to_bool (Jv.get raw "ctrlKey") || Jv.to_bool (Jv.get raw "metaKey") then begin
           ignore @@ Jv.call raw "preventDefault" [||];
           do_zoom ~origin:(Jv.to_float (Jv.get raw "clientY"))
             (!zoom *. exp (-. Jv.to_float (Jv.get raw "deltaY") /. 100.))
         end);
       Jv.obj [| "passive", Jv.false' |] |];

  (* — keyboard — *)
  ignore @@ Ev.listen Ev.keydown (fun ev ->
    let raw = Ev.to_jv ev in
    let key = Jstr.to_string (Ev.as_type ev |> Ev.Keyboard.key) in
    let accel = Jv.to_bool (Jv.get raw "ctrlKey") || Jv.to_bool (Jv.get raw "metaKey") in
    let prevent () = ignore @@ Jv.call raw "preventDefault" [||] in
    let b = El.to_jv (body ()) in
    let scroll_by dy = Jv.set b "scrollTop"
      (Jv.of_float (Jv.to_float (Jv.get b "scrollTop") +. dy)) in
    match key with
    | "Escape" ->
      close sidebar toc_btn; close info_overlay info_btn
    | "=" | "+" when accel -> prevent (); do_zoom (!zoom *. 1.1)
    | "-" when accel -> prevent (); do_zoom (!zoom /. 1.1)
    | "0" when accel -> prevent (); do_zoom 1.0
    | "Home" -> Jv.set b "scrollTop" (Jv.of_int 0)
    | "End" -> Jv.set b "scrollTop" (Jv.get b "scrollHeight")
    | "ArrowLeft" | "PageUp" -> scroll_by (-800.)
    | "ArrowRight" | "PageDown" -> scroll_by 800.
    | _ -> ()
  ) (Window.as_target G.window);

  (* — restore state — *)
   (match ss_get "epub-ml-state" with
   | Some s ->
     (match String.split_on_char ',' s with
      | scroll_s :: zoom_s :: rest ->
        zoom := float_of_string zoom_s; update_zoom ();
        (match rest with "1" :: _ -> toggle sidebar toc_btn | _ -> ());
        raf (fun _ ->
          Jv.set (El.to_jv (body ())) "scrollTop" (Jv.of_float (float_of_string scroll_s));
          update_progress ())
      | _ -> update_progress ())
   | None -> update_progress ())

and cache_blob blob =
  let caches = Jv.get Jv.global "caches" in
  let* cache = Fut.of_promise ~ok:Fun.id
    (Jv.call caches "open" [| Jv.of_string "epub-ml" |]) in
  (match cache with
   | Ok c ->
     let resp = Jv.new' (Jv.get Jv.global "Response") [| Blob.to_jv blob |] in
     ignore @@ Jv.call c "put" [| Jv.of_string tab_key; resp |]
   | Error _ -> ());
  Fut.return ()

and open_blob ?(from_cache=false) blob =
  let*? ab = Blob.array_buffer blob in
  let ( let* ) = Result.bind in
  let s = Tarray.to_string (Tarray.of_buffer Tarray.Uint8 ab) in
  (match let* z = Zipc.of_binary_string s in let* epub = Epub.of_zip z in Ok (epub, z) with
   | Error e -> Console.(log [js e])
   | Ok (epub, z) ->
     ignore @@ cache_blob blob;
     if not from_cache then begin
       ss_set "epub-ml-state" "";
       Window.History.push_state ~state:(Jv.of_string "viewer") (Window.history G.window)
     end;
     Hashtbl.clear resource_cache;
     setup_viewer epub z);
  Fut.return ()

and show_picker () =
  Jv.set (Document.to_jv G.document) "title" (Jv.of_string "epub.ml");
  let b = body () in
  El.set_class (js "viewer") false b;
  El.set_class (js "picker") true b;
  let input = El.v (js "input")
    ~at:[At.v (js "type") (js "file"); At.v (js "accept") (js ".epub,application/epub+zip");
         At.v (js "style") (js "display:none")] [] in
  let btn = El.v (js "button") ~at:[At.v (js "id") (js "pickButton")] [El.txt' "open"] in
  El.set_children b [El.v (js "div") ~at:[At.v (js "id") (js "picker")]
    [El.v (js "p") [El.txt' "epub.ml"]; btn; input;
     El.v (js "span") ~at:[At.class' (js "picker-links")]
       [El.txt' "Source: ";
        El.v (js "a") ~at:[At.v (js "href") (js "https://github.com/nadja-y/epub.ml")]
          [El.txt' "https://github.com/nadja-y/epub.ml"];
        El.v (js "br") [];
        El.txt' "License: ";
        El.v (js "a") ~at:[At.v (js "href") (js "https://www.gnu.org/licenses/old-licenses/gpl-2.0.html")]
          [El.txt' "https://www.gnu.org/licenses/old-licenses/gpl-2.0.html"]]]];
  ignore @@ Ev.listen Ev.click (fun _ev -> El.click input) (El.as_target btn);
  ignore @@ Ev.listen Ev.change (fun _ev ->
    let files = Jv.get (El.to_jv input) "files" in
    if Jv.to_int (Jv.get files "length") > 0 then
      ignore @@ open_blob (Blob.of_jv (Jv.call files "item" [| Jv.of_int 0 |]))
  ) (El.as_target input)

and try_cache () =
  let caches = Jv.get Jv.global "caches" in
  let* cache = Fut.of_promise ~ok:Fun.id
    (Jv.call caches "open" [| Jv.of_string "epub-ml" |]) in
  match cache with
  | Error _ -> show_picker (); Fut.return ()
  | Ok c ->
    let* resp = Fut.of_promise ~ok:Fun.id
      (Jv.call c "match" [| Jv.of_string tab_key |]) in
    match resp with
    | Ok r when not (Jv.is_undefined r) ->
      let* blob = Fut.of_promise ~ok:Blob.of_jv (Jv.call r "blob" [||]) in
      (match blob with
       | Ok b -> open_blob ~from_cache:true b
       | Error _ -> show_picker (); Fut.return ())
    | _ -> show_picker (); Fut.return ()

let () = ignore @@ try_cache ()
