{ mkDerivation, aeson, base, base-compat-batteries, bytestring
, Cabal, cabal-doctest, containers, doctest, generics-sop, Glob
, hashable, hspec, http-media, HUnit, insert-ordered-containers
, lens, mtl, network, QuickCheck, quickcheck-instances, scientific
, stdenv, template-haskell, text, time, transformers
, transformers-compat, unordered-containers, utf8-string
, uuid-types, vector
}:
mkDerivation {
  pname = "swagger2";
  version = "2.3.0.1";
  sha256 = "2ee44e05a953c5a25f0e948a89bfdac9940550a31c29fbf2c0135178c58e17d1";
  revision = "2";
  editedCabalFile = "0dfxf47mzzb5rmln2smsk0qx53kj1lc3a087r52g2rzz6971zivb";
  setupHaskellDepends = [ base Cabal cabal-doctest ];
  libraryHaskellDepends = [
    aeson base base-compat-batteries bytestring containers generics-sop
    hashable http-media insert-ordered-containers lens mtl network
    scientific template-haskell text time transformers
    transformers-compat unordered-containers uuid-types vector
  ];
  testHaskellDepends = [
    aeson base base-compat-batteries bytestring containers doctest Glob
    hashable hspec HUnit insert-ordered-containers lens mtl QuickCheck
    quickcheck-instances template-haskell text time
    unordered-containers utf8-string vector
  ];
  homepage = "https://github.com/GetShopTV/swagger2";
  description = "Swagger 2.0 data model";
  license = stdenv.lib.licenses.bsd3;
}
