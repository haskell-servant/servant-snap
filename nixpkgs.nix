{ overlays }:
let
  nixpkgs = import <nixpkgs> {};
  fixedNixpkgs = nixpkgs.fetchFromGitHub {
         owner = "NixOS";
         repo = "nixpkgs-channels";
         rev = "4dd5c93998da55002fdec1c715c680531420381c";
         sha256  = "06paxakic36nbdnwkkb1094fzp3lpzxxb1r57gmb3py6pb6xrcnh";
   };
in
  import fixedNixpkgs { inherit overlays; }
