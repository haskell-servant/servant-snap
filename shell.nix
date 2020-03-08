{ compiler ? "default"
, nixpkgs  ? null
}:
let
  rel = import ./release.nix { inherit compiler nixpkgs; };
in
  rel.servant-snap.env
