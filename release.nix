let
  pkgs = import ./pkgs.nix;
in {
  servant-snap = pkgs.haskellPackages.callPackage ./default.nix {};
}
