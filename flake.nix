{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

  outputs =
    { self, nixpkgs }:
    let
      lib = nixpkgs.lib;
      forAllSystems = lib.genAttrs lib.systems.flakeExposed;
    in
    {
      overlays.default = final: prev: {
        ocamlPackages = prev.ocamlPackages.overrideScope (
          ofinal: oprev: {
            epub-ml = ofinal.callPackage ./package.nix { };
          }
        );
        epub-ml-site = final.callPackage ./site.nix { };
      };

      packages = forAllSystems (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
        in
        {
          default = pkgs.callPackage ./site.nix {
            ocamlPackages = pkgs.ocamlPackages.overrideScope (
              ofinal: oprev: {
                epub-ml = ofinal.callPackage ./package.nix { };
              }
            );
          };
          lib = pkgs.ocamlPackages.callPackage ./package.nix { };
        }
      );
    };
}
