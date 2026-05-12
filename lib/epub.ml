(* SPDX-License-Identifier: GPL-2.0-only *)
(* epub.ml - EPUB 3.3 container parser *)

let ( let* ) = Result.bind

module Xml = struct
  type t = El of Xmlm.tag * t list | Data of string

  let parse s =
    let inp = Xmlm.make_input (`String (0, s)) in
    try Ok (snd (Xmlm.input_doc_tree
                   ~el:(fun tag cs -> El (tag, cs))
                   ~data:(fun s -> Data s) inp))
    with Xmlm.Error ((l, c), e) ->
      Error (Printf.sprintf "%d:%d: %s" l c (Xmlm.error_message e))

  let name = function El (((_ns, n), _), _) -> n | Data _ -> ""
  let attrs = function El ((_, a), _) -> a | Data _ -> []
  let children = function El (_, cs) -> cs | Data _ -> []

  let rec text_content = function
    | Data s -> s
    | El (_, cs) -> String.concat "" (List.map text_content cs)

  let attr k el =
    List.find_map (fun ((_, n), v) ->
      if n = k then Some v else None) (attrs el)

  let attr_or k ~default el = Option.value ~default (attr k el)
  let find_child n el = List.find_opt (fun c -> name c = n) (children el)
  let find_children n el = List.filter (fun c -> name c = n) (children el)

  let rec find_by p = function
    | El _ as el when p el -> Some el
    | El (_, cs) -> List.find_map (find_by p) cs
    | Data _ -> None

  let find_element n = find_by (fun c -> name c = n)
end

type resource = {
  id : string;
  href : string;
  media_type : string;
  properties : string list;
}

type spine_item = {
  idref : string;
  linear : bool;
  resource : resource;
}

type metadata = {
  identifiers : string list;
  titles : string list;
  languages : string list;
  modified : string;
  creators : string list;
  contributors : string list;
  publishers : string list;
  date : string;
  description : string;
  rights : string;
  subjects : string list;
  source : string;
}

type toc_entry = {
  label : string;
  path : string;
  fragment : string option;
}

type t = {
  metadata : metadata;
  spine : spine_item list;
  resources : resource list;
  nav_href : string option;
  cover_href : string option;
  base_path : string;
}

let normalize_path path =
  let rec go acc = function
    | [] -> List.rev acc
    | "." :: rest -> go acc rest
    | ".." :: rest -> go (match acc with [] -> [] | _ :: tl -> tl) rest
    | "" :: rest when acc <> [] -> go acc rest
    | seg :: rest -> go (seg :: acc) rest
  in
  String.split_on_char '/' path |> go [] |> String.concat "/"

let resolve_href base href =
  normalize_path
    (if String.starts_with ~prefix:"/" href
     then String.sub href 1 (String.length href - 1)
     else base ^ href)

let dirname s =
  match String.rindex_opt s '/' with
  | None -> "" | Some i -> String.sub s 0 (i + 1)

let max_member_size = 100_000_000

let zip_read z path =
  let p = normalize_path path in
  match Zipc.find p z with
  | None -> Error ("missing: " ^ p)
  | Some m ->
    match Zipc.Member.kind m with
    | Zipc.Member.Dir -> Error ("directory: " ^ p)
    | Zipc.Member.File f ->
      if Zipc.File.decompressed_size f > max_member_size
      then Error ("too large: " ^ p)
      else Zipc.File.to_binary_string f
           |> Result.map_error (fun e -> p ^ ": " ^ e)

let split_ws s =
  let b = Buffer.create 16 and acc = ref [] in
  String.iter (function
    | ' ' | '\t' | '\n' | '\r' ->
      if Buffer.length b > 0 then
        (acc := Buffer.contents b :: !acc; Buffer.clear b)
    | c -> Buffer.add_char b c) s;
  if Buffer.length b > 0 then acc := Buffer.contents b :: !acc;
  List.rev !acc

let require msg = function Some x -> Ok x | None -> Error msg

let find_opf_path z =
  let* data = zip_read z "META-INF/container.xml" in
  let* tree = Xml.parse data in
  let* rf = require "no rootfile in container.xml"
              (Xml.find_element "rootfile" tree) in
  if Xml.attr_or "media-type" ~default:"" rf <> "application/oebps-package+xml"
  then Error "rootfile has wrong media-type"
  else require "rootfile missing full-path" (Xml.attr "full-path" rf)

let parse_manifest base el =
  let tbl = Hashtbl.create 64 in
  List.iter (fun item ->
    let id = Xml.attr_or "id" ~default:"" item in
    if id <> "" then
      Hashtbl.replace tbl id {
        id;
        href = resolve_href base (Xml.attr_or "href" ~default:"" item);
        media_type = Xml.attr_or "media-type" ~default:"" item;
        properties = (match Xml.attr "properties" item with
          | Some s -> split_ws s | None -> []);
      }
  ) (Xml.find_children "item" el);
  tbl

let parse_spine manifest el =
  Xml.find_children "itemref" el |> List.filter_map (fun ir ->
    let idref = Xml.attr_or "idref" ~default:"" ir in
    Hashtbl.find_opt manifest idref
    |> Option.map (fun r ->
      { idref;
        linear = Xml.attr_or "linear" ~default:"yes" ir = "yes";
        resource = r }))

let dc_text name el =
  Xml.find_children name el |> List.filter_map (fun c ->
    let s = String.trim (Xml.text_content c) in
    if s = "" then None else Some s)

let parse_metadata el =
  let modified =
    Xml.find_children "meta" el |> List.find_map (fun m ->
      if Xml.attr "property" m = Some "dcterms:modified"
      then Some (String.trim (Xml.text_content m))
      else None)
    |> Option.value ~default:""
  in
  let ids = dc_text "identifier" el
  and titles = dc_text "title" el
  and langs = dc_text "language" el in
  if ids = [] then Error "missing dc:identifier"
  else if titles = [] then Error "missing dc:title"
  else if langs = [] then Error "missing dc:language"
  else Ok {
    identifiers = ids; titles; languages = langs; modified;
    creators = dc_text "creator" el;
    contributors = dc_text "contributor" el;
    publishers = dc_text "publisher" el;
    date = (match dc_text "date" el with d :: _ -> d | [] -> "");
    description = (match dc_text "description" el with d :: _ -> d | [] -> "");
    rights = (match dc_text "rights" el with d :: _ -> d | [] -> "");
    subjects = dc_text "subject" el;
    source = (match dc_text "source" el with d :: _ -> d | [] -> "");
  }

let find_prop prop manifest =
  Hashtbl.fold (fun _ r -> function
    | Some _ as found -> found
    | None -> if List.mem prop r.properties then Some r.href else None
  ) manifest None

let parse_opf z opf_path =
  let* data = zip_read z opf_path in
  let base = dirname opf_path in
  let* tree = Xml.parse data in
  let* meta_el = require "missing metadata" (Xml.find_child "metadata" tree) in
  let* manifest_el = require "missing manifest" (Xml.find_child "manifest" tree) in
  let* spine_el = require "missing spine" (Xml.find_child "spine" tree) in
  let* metadata = parse_metadata meta_el in
  let manifest = parse_manifest base manifest_el in
  Ok {
    metadata;
    spine = parse_spine manifest spine_el;
    resources = Hashtbl.fold (fun _ r acc -> r :: acc) manifest [];
    nav_href = find_prop "nav" manifest;
    cover_href = find_prop "cover-image" manifest;
    base_path = base;
  }

let of_zip z =
  let* opf_path = find_opf_path z in
  parse_opf z opf_path

let read_spine_item z item = zip_read z item.resource.href
let read_raw z path = zip_read z path

type nav = {
  toc : toc_entry list;
  page_list : toc_entry list;
}

let parse_nav base_path xhtml =
  match Xml.parse xhtml with
  | Error _ -> { toc = []; page_list = [] }
  | Ok tree ->
    let open Xml in
    let rec extract_links = function
      | El (((_ns, "a"), _), _) as a ->
        let raw = attr_or "href" ~default:"" a in
        let path, fragment = match String.index_opt raw '#' with
          | None -> raw, None
          | Some i -> String.sub raw 0 i,
                      Some (String.sub raw (i + 1) (String.length raw - i - 1)) in
        [{ label = String.trim (text_content a);
           path = resolve_href base_path path;
           fragment }]
      | El (_, cs) -> List.concat_map extract_links cs
      | Data _ -> []
    in
    let nav_of_type typ =
      find_by (fun el -> name el = "nav" && attr "type" el = Some typ) tree
      |> Option.map (fun el -> List.concat_map extract_links (children el))
      |> Option.value ~default:[]
    in
    { toc = nav_of_type "toc";
      page_list = nav_of_type "page-list" }
