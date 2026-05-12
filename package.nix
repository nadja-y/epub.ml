{
  lib,
  buildDunePackage,
  xmlm,
  zipc,
}:

buildDunePackage {
  pname = "epub-ml";
  version = "0.1.0";
  src = ./.;

  duneVersion = "3";

  propagatedBuildInputs = [
    xmlm
    zipc
  ];

  meta = {
    description = "EPUB 3.3 parser for OCaml";
    homepage = "https://github.com/nadja-y/epub.ml";
    license = lib.licenses.gpl2Only;
    maintainers = [ lib.maintainers.nadja-y ];
  };
}
