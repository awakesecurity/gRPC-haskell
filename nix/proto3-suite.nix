{ mkDerivation, aeson, aeson-pretty, attoparsec, base
, base64-bytestring, bytestring, cereal, containers, deepseq
, doctest, fetchgit, foldl, haskell-src, lens, mtl
, neat-interpolation, optparse-generic, parsec, parsers, pretty
, pretty-show, proto3-wire, QuickCheck, range-set-list, safe
, semigroups, stdenv, swagger2, system-filepath, tasty, tasty-hunit
, tasty-quickcheck, text, transformers, turtle, vector
}:
mkDerivation {
  pname = "proto3-suite";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/awakenetworks/proto3-suite";
    sha256 = "1q7fyavirkb5lh8qic4sc67jbakafr8msj6k2i29drzfpv2gbvzq";
    rev = "bb66853dcaa7372ef061cf83f39e4ba5193898de";
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-pretty attoparsec base base64-bytestring bytestring
    cereal containers deepseq foldl haskell-src lens mtl
    neat-interpolation parsec parsers pretty pretty-show proto3-wire
    QuickCheck safe semigroups swagger2 system-filepath text
    transformers turtle vector
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
