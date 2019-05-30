{ mkDerivation, aeson, attoparsec, base, base64-bytestring
, bytestring, case-insensitive, containers, digestive-functors
, directory, either, errors, exceptions, filepath, heist, hspec
, hspec-core, hspec-snap, http-api-data, http-types, HUnit
, io-streams, lens, map-syntax, mmorph, mtl, network, network-uri
, parsec, process, QuickCheck, servant, snap, snap-core, snap-cors
, snap-server, stdenv, string-conversions, temporary, text, time
, transformers, word8
}:
mkDerivation {
  pname = "servant-snap";
  version = "0.8.4.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson attoparsec base base64-bytestring bytestring case-insensitive
    containers either filepath http-api-data http-types io-streams
    mmorph mtl network-uri servant snap snap-core snap-server
    string-conversions text transformers word8
  ];
  executableHaskellDepends = [
    aeson base bytestring either errors heist lens map-syntax servant
    snap snap-core snap-cors snap-server text transformers
  ];
  testHaskellDepends = [
    aeson base base64-bytestring bytestring case-insensitive containers
    digestive-functors directory either exceptions hspec hspec-core
    hspec-snap http-types HUnit lens mtl network parsec process
    QuickCheck servant snap snap-core snap-cors snap-server
    string-conversions temporary text time transformers
  ];
  homepage = "http://haskell-servant.github.io/";
  description = "A family of combinators for defining webservices APIs and serving them";
  license = stdenv.lib.licenses.bsd3;
}
