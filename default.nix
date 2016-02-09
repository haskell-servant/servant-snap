{ mkDerivation, aeson, attoparsec, base, blaze-builder, bytestring
, bytestring-conversion, case-insensitive, containers
, contravariant, directory, either, errors, exceptions, filepath
, heist, hspec, hspec-core, hspec-snap, http-api-data, http-types
, io-streams, lens, map-syntax, mmorph, mtl, network, network-uri
, parsec, QuickCheck, safe, servant, servant-blaze, snap, snap-core
, snap-server, split, stdenv, string-conversions, system-filepath
, temporary, text, transformers
}:
mkDerivation {
  pname = "servant-snap";
  version = "0.5";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson attoparsec base blaze-builder bytestring case-insensitive
    containers contravariant either filepath http-api-data http-types
    io-streams mmorph mtl network-uri safe servant snap snap-core
    snap-server split string-conversions system-filepath text
    transformers
  ];
  executableHaskellDepends = [
    aeson base blaze-builder bytestring either errors heist lens
    map-syntax servant servant-blaze snap snap-core snap-server text
    transformers
  ];
  testHaskellDepends = [
    aeson base bytestring bytestring-conversion containers directory
    either exceptions hspec hspec-core hspec-snap http-types mtl
    network parsec QuickCheck servant snap snap-core snap-server
    string-conversions temporary text transformers
  ];
  homepage = "http://haskell-servant.github.io/";
  description = "A family of combinators for defining webservices APIs and serving them";
  license = stdenv.lib.licenses.bsd3;
}
