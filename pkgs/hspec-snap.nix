{ mkDerivation, aeson, base, bytestring, containers
, digestive-functors, directory, fetchgit, HandsomeSoup, hspec
, hspec-core, HUnit, hxt, lens, mtl, snap, snap-core, stdenv, text
, transformers
}:
mkDerivation {
  pname = "hspec-snap";
  version = "1.0.0.2";
  src = fetchgit {
    url = "http://github.com/imalsogreg/hspec-snap";
    sha256 = "0mrz5mn8l33pybwhjq2rv0i13m8mrkin0v4fpwr10c4f456ng8zm";
    rev = "9c661b370805f2ecf5cc22252e9fd5719bed0b16";
  };
  libraryHaskellDepends = [
    aeson base bytestring containers digestive-functors HandsomeSoup
    hspec hspec-core HUnit hxt lens mtl snap snap-core text
    transformers
  ];
  testHaskellDepends = [
    aeson base bytestring containers digestive-functors directory
    HandsomeSoup hspec hspec-core hxt lens mtl snap snap-core text
    transformers
  ];
  homepage = "https://github.com/dbp/hspec-snap";
  description = "A library for testing with Hspec and the Snap Web Framework";
  license = stdenv.lib.licenses.bsd3;
}
