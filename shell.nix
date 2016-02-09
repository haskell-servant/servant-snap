let
  pkgs = import <nixpkgs> {};

  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: {
      servant-cassava = pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.appendConfigureFlag (self.callPackage deps/servant/servant-cassava {}) "--ghc-options=-Werror");
      servant-client = pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.appendConfigureFlag (self.callPackage deps/servant/servant-client {}) "--ghc-options=-Werror");
      servant-docs = pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.appendConfigureFlag (self.callPackage deps/servant/servant-docs {}) "--ghc-options=-Werror");
      servant-foreign = pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.appendConfigureFlag (self.callPackage deps/servant/servant-foreign {}) "--ghc-options=-Werror");
      servant-js = pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.appendConfigureFlag (self.callPackage deps/servant/servant-js {}) "--ghc-options=-Werror");
      servant-server = pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.appendConfigureFlag (self.callPackage deps/servant/servant-server {}) "--ghc-options=-Werror");
      servant-examples = pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.appendConfigureFlag (self.callPackage deps/servant/servant-examples {}) "--ghc-options=-Werror");
      servant-blaze = pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.appendConfigureFlag (self.callPackage deps/servant/servant-blaze {}) "--ghc-options=-Werror");
      servant-lucid = pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.appendConfigureFlag (self.callPackage deps/servant/servant-lucid {}) "--ghc-options=-Werror");
      servant-mock = pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.appendConfigureFlag (self.callPackage deps/servant/servant-mock {}) "--ghc-options=-Werror");
      servant = pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.appendConfigureFlag (self.callPackage deps/servant/servant {}) "--ghc-options=-Werror");
      servant-snap = pkgs.haskell.lib.appendConfigureFlag (self.callPackage ./. {}) "--ghc-options=-Werror";
    };
  };

in haskellPackages.servant-snap.env
