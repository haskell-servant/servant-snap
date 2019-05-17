{ overlays }:
let
  nixpkgs = import <nixpkgs> {};
  fixedNixpkgs = nixpkgs.fetchFromGitHub {
         owner = "NixOS";
         repo = "nixpkgs-channels";
         rev = "bc94dcf500286495e3c478a9f9322debc94c4304";
         sha256  = "1siqklf863181fqk19d0x5cd0xzxf1w0zh08lv0l0dmjc8xic64a";
   };
in
  import fixedNixpkgs { inherit overlays; }
