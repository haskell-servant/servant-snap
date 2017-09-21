{ compiler ? "default" }:
let
  pkgs = import ./pkgs.nix;
  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

in {
  servant-snap = haskellPackages.callPackage ./default.nix {};
}
