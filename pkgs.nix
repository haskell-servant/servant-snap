let
  overlay = pself: pkgs:
    let
      dontCheck = pkgs.haskell.lib.dontCheck;
    in {
      haskellPackages = pkgs.haskell.packages.ghc802.override {
        overrides = self: super: {
          #hspec-snap = dontCheck (self.callPackage ./pkgs/hspec-snap.nix {});
          lens        = dontCheck super.lens;
          servant     = dontCheck super.servant;
        };
      };
    };
in import ./nixpkgs.nix { overlays = [overlay]; }
