{ overlays }:
let
  nixpkgs = import <nixpkgs> {};
  fixedNixpkgs = nixpkgs.fetchFromGitHub {
         owner = "NixOS";
         repo = "nixpkgs-channels";
         rev = "53835c93cb4bc1c6228ee04d6788398a8ab36ab4";
         sha256  = "0yf6pvdlqxzlhbndd2ri10rkzhjx4bg32hwb9ykhbbwy47164i8l";
   };
in
  import fixedNixpkgs { inherit overlays; }