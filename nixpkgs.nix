{ overlays }:
let
  nixpkgs = import <nixpkgs> {};
  fixedNixpkgs = nixpkgs.fetchFromGitHub {
         owner = "NixOS";
         repo = "nixpkgs-channels";
         rev = "b96cd4134a8ae2fd8d37a60acc6e4921191e2818";
         sha256  = "15vz6q0fy2xrpk1jwhdy2v7abg388sjqyjznr643gn2xg6k1nha2";
   };
in
  import fixedNixpkgs { inherit overlays; }
