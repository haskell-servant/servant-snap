{ pkgs ? import <nixpkgs> {},
  compilerVersion ? "ghc865"
}:

pkgs.haskell.packages."${compilerVersion}".developPackage {
  name = "servant-snap";
  root = pkgs.lib.cleanSourceWith
    {
      src = ./.;
      filter = path: type:
        !(baseNameOf (toString path) == "dist-newstyle");

    };
}
