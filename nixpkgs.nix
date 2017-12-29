{ overlays }:
let
  nixpkgs = import <nixpkgs> {};
  fixedNixpkgs = nixpkgs.fetchFromGitHub {
         owner = "NixOS";
         repo = "nixpkgs-channels";
         rev = "ade98dc442ea78e9783d5e26954e64ec4a1b2c94";
         sha256  = "0dq2ymqygc6dadrlm1jcbqsg7w34yihb7gss9yk42lknajzvm9pm";
   };
in
  import fixedNixpkgs { inherit overlays; }
