(lang dune 3.17)
(name epub-ml)

(generate_opam_files true)

(package
 (name epub-ml)
 (synopsis "EPUB viewer")
  (depends
   (ocaml (>= 5.1))
   xmlm
   zipc
   js_of_ocaml-ppx
   brr))
