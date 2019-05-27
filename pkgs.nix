let
  overlay = pself: pkgs:
    let
      dontCheck = pkgs.haskell.lib.dontCheck;
      doJailbreak = pkgs.haskell.lib.doJailbreak;
      servant-src = (pkgs.fetchFromGitHub {
                          owner  = "haskell-servant";
                          repo   = "servant";
                          rev    = "9a1856502721100c1d3e6b4233004172bcfa35ff";
                          sha256 = "0kqglih3rv12nmkzxvalhfaaafk4b2irvv9x5xmc48i1ns71y23l";
                        });
      hspec-snap-src = pkgs.fetchFromGitHub {
                         owner  = "dbp";
                         repo   = "hspec-snap";
                         rev    = "d13e3bd28b546728402e8e1ea2343b721c4b7dda";
                         sha256 = "1x155cpb8fqkywvbanpankbly5410ly8v1nngpnmw8mnqldbfwd6";
                        };
    in {
      # haskellPackages = pkgs.haskell.packages.ghc822.override {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = self: super: {
          hspec-snap = dontCheck (self.callCabal2nix "hspec-snap" hspec-snap-src {});
          # hspec-snap = doJailbreak super.hspec-snap;
          lens        = dontCheck super.lens;
          # servant     = dontCheck self."servant-0.12";
          servant     = doJailbreak (dontCheck (self.callCabal2nix "servant" (servant-src + "/servant") {}));
          servant-client     = doJailbreak (dontCheck (self.callCabal2nix "servant-client" (servant-src + "/servant-client") {}));
          servant-client-core     = doJailbreak (dontCheck (self.callCabal2nix "servant-client-core" (servant-src + "/servant-client-core") {}));
          servant-server     = doJailbreak (dontCheck (self.callCabal2nix "servant-server" (servant-src + "/servant-server") {}));
          # servant       = self.servant_0_13;
          # servant-client       = self.servant-client_0_13;
          # servant-server       = self.servant-server_0_13;
          heist       = dontCheck (super.heist);
          # text        = self.text_1_2_3_0;
          # http-types  = self.http-types_0_12_1;
          map-syntax = doJailbreak super.map-syntax;
        };
      };
    };
in import ./nixpkgs.nix { overlays = [overlay]; }
