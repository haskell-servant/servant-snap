let
  overlay = pself: pkgs:
    let
      dontCheck = pkgs.haskell.lib.dontCheck;
      doJailbreak = pkgs.haskell.lib.doJailbreak;
      servant-src = (pkgs.fetchFromGitHub {
                          owner  = "haskell-servant";
                          repo   = "servant";
                          rev    = "b57528eff2c69feee2bb5dc43f37fa75b9daf417";
                          sha256 = "1dgmqp9lw9zkb47r71568hlm86gvs0mmd8amc5sh7xcy220q7rbf";
                        }) + "/servant";
    in {
      # haskellPackages = pkgs.haskell.packages.ghc822.override {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = self: super: {
          #hspec-snap = dontCheck (self.callPackage ./pkgs/hspec-snap.nix {});
          hspec-snap = doJailbreak super.hspec-snap;
          lens        = dontCheck super.lens;
          # servant     = dontCheck self."servant-0.12";
          # servant     = dontCheck (self.callCabal2nix "servant" servant-src {});
          servant       = self.servant_0_13;
          servant-client       = self.servant-client_0_13;
          servant-server       = self.servant-server_0_13;
          heist       = dontCheck (super.heist);
          text        = self.text_1_2_3_0;
          http-types  = self.http-types_0_12_1;
        };
      };
    };
in import ./nixpkgs.nix { overlays = [overlay]; }
