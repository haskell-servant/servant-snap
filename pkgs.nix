let
  overlay = pself: pkgs:
    let
      dontCheck = pkgs.haskell.lib.dontCheck;
      doJailbreak = pkgs.haskell.lib.doJailbreak;
      servant-src = (pkgs.fetchFromGitHub {
                          owner  = "haskell-servant";
                          repo   = "servant";
                          rev    = "b17c8bb8bd59ef8341bad07f9f7d0e230603612b";
                          sha256 = "0dyn50gidzbgyq9yvqijnysai9hwd3srqvk8f8rykh09l375xb9j";
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
          #hspec-snap = dontCheck (self.callPackage ./pkgs/hspec-snap.nix {});
          hspec-snap = self.callCabal2nix "hspec-snap" hspec-snap-src {};
          # hspec-snap = doJailbreak super.hspec-snap;
          lens        = dontCheck super.lens;
          map-syntax  = dontCheck super.map-syntax;

          # For servant-0.16. Comment it out to get servant-0.15:
          # servant     = dontCheck (self.callCabal2nix "servant" (servant-src + "/servant") {});
          # servant-client-core   = dontCheck (self.callCabal2nix "servant-client-core" (servant-src + "/servant-client-core") {});
          # servant-client   = dontCheck (self.callCabal2nix "servant-client" (servant-src + "/servant-client") {});


          # servant       = self.servant_0_13;
          # servant-client       = self.servant-client_0_16;
          # servant-server       = self.servant-server_0_16;
          heist       = dontCheck (super.heist);
          # text        = self.text_1_2_3_0;
          # http-types  = self.http-types_0_12_1;
        };
      };
    };
in import ./nixpkgs.nix { overlays = [overlay]; }
