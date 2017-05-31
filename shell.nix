let
  rel = import ./release.nix;
in
  rel.servant-snap.env
