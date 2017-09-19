{ overlays }:
let
  nixpkgs = import <nixpkgs> {};
  fixedNixpkgs = nixpkgs.fetchFromGitHub {
         owner = "NixOS";
         repo = "nixpkgs-channels";
         rev = "c8e7aab0c8bae8a49ec5bd87ace65b237c8e3d18";
         sha256  = "0dq2ymqygc6dadrlm1jcbqsg7w34yihb7gss9yk42lknajzvm9pm";
   };
in
  import fixedNixpkgs { inherit overlays; }
