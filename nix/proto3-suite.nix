{ mkDerivation, aeson, attoparsec, base, base64-bytestring
, bytestring, cereal, containers, deepseq, doctest, fetchgit, foldl
, haskell-src, lens, mtl, neat-interpolation, optparse-generic
, parsec, parsers, pretty, pretty-show, proto3-wire, QuickCheck
, range-set-list, safe, semigroups, stdenv, system-filepath, tasty
, tasty-hunit, tasty-quickcheck, text, transformers, turtle, vector
}:
mkDerivation {
  pname = "proto3-suite";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/awakenetworks/proto3-suite.git";
    sha256 = "1hvp9ci5r1c6rjc85bqnnhw5rk4hgrmqbzc0662wimnc54y2hqqk";
    rev = "e8f2acb2b48a0f1c1eadbfdb5c8351e2ac8557bb";
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson attoparsec base base64-bytestring bytestring cereal
    containers deepseq foldl haskell-src lens mtl neat-interpolation
    parsec parsers pretty pretty-show proto3-wire QuickCheck safe
    semigroups system-filepath text transformers turtle vector
  ];
  executableHaskellDepends = [
    base containers optparse-generic proto3-wire range-set-list
    system-filepath text turtle
  ];
  testHaskellDepends = [
    aeson attoparsec base base64-bytestring bytestring cereal doctest
    pretty-show proto3-wire QuickCheck semigroups tasty tasty-hunit
    tasty-quickcheck text transformers turtle vector
  ];
  description = "A low level library for writing out data in the Protocol Buffers wire format";
  license = stdenv.lib.licenses.asl20;
}
