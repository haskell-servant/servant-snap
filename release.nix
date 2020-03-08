{ compiler ? "default"
, nixpkgs  ? null
}:
let
  pkgs = import ./pkgs.nix { inherit nixpkgs; };
  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

in {
  servant-snap = haskellPackages.callPackage ./default.nix {};
}
