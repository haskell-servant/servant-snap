{ compiler ? "default" }:
let
  rel = import ./release.nix { compiler = compiler; };
in
  rel.servant-snap.env
