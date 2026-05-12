{
  lib,
  stdenv,
  ocamlPackages,
  fraunces,
  woff2,
  closurecompiler,
}:

stdenv.mkDerivation {
  pname = "epub-ml-site";
  version = "0.1.0";
  src = ./.;

  nativeBuildInputs = [
    woff2
    closurecompiler
  ]
  ++ (with ocamlPackages; [
    dune_3
    ocaml
    findlib
    js_of_ocaml-compiler
    js_of_ocaml-ppx
  ]);

  buildInputs = with ocamlPackages; [
    epub-ml
    js_of_ocaml
    js_of_ocaml-ppx
    brr
  ];

  buildPhase = ''
    runHook preBuild
    dune build ext/viewer.bc.js

    mkdir -p ext/viewer/fonts
    cp "${fraunces}/share/fonts/truetype/Fraunces-Italic[SOFT,WONK,opsz,wght].ttf" ext/viewer/fonts/fraunces-italic.ttf
    woff2_compress ext/viewer/fonts/fraunces-italic.ttf
    rm ext/viewer/fonts/fraunces-italic.ttf
    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall

    mkdir -p $out
    closure-compiler \
      --compilation_level SIMPLE \
      --js_output_file $out/viewer.bc.js \
      --language_in ECMASCRIPT_NEXT \
      --language_out ECMASCRIPT_2020 \
      --warning_level QUIET \
      _build/default/ext/viewer.bc.js
    cp ext/viewer/viewer.html $out/index.html
    cp ext/viewer/viewer.css $out/
    cp ext/icon.svg $out/
    cp -r ext/viewer/fonts $out/

    runHook postInstall
  '';

  meta = {
    description = "EPUB viewer";
    homepage = "https://github.com/nadja-y/epub.ml";
    license = lib.licenses.gpl2Only;
    maintainers = [ lib.maintainers.nadja-y ];
  };
}
