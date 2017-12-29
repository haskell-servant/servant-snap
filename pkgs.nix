let
  overlay = pself: pkgs:
    let
      dontCheck = pkgs.haskell.lib.dontCheck;
      servant-src = (pkgs.fetchFromGitHub {
                          owner  = "haskell-servant";
                          repo   = "servant";
                          rev    = "b57528eff2c69feee2bb5dc43f37fa75b9daf417";
                          sha256 = "1dgmqp9lw9zkb47r71568hlm86gvs0mmd8amc5sh7xcy220q7rbf";
                        }) + "/servant";
    in {
      haskellPackages = pkgs.haskell.packages.ghc802.override {
        overrides = self: super: {
          #hspec-snap = dontCheck (self.callPackage ./pkgs/hspec-snap.nix {});
          lens        = dontCheck super.lens;
          #servant     = dontCheck self."servant-0.12";
          servant     = dontCheck (self.callCabal2nix "servant" servant-src {});
        };
      };
    };
in import ./nixpkgs.nix { overlays = [overlay]; }
