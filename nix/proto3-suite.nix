{ mkDerivation, aeson, aeson-pretty, attoparsec, base
, base64-bytestring, binary, bytestring, cereal, containers
, contravariant, deepseq, dhall, doctest, fetchgit, filepath, foldl
, generic-arbitrary, ghc, hashable, hedgehog
, insert-ordered-containers, large-generics, large-records, lens
, lib, mtl, neat-interpolation, optparse-applicative
, optparse-generic, parsec, parsers, pretty, pretty-show
, proto3-wire, QuickCheck, quickcheck-instances, range-set-list
, record-hasfield, safe, split, swagger2, system-filepath, tasty
, tasty-hedgehog, tasty-hunit, tasty-quickcheck, text, text-short
, time, transformers, turtle, vector
}:
mkDerivation {
  pname = "proto3-suite";
  version = "0.8.2";
  src = fetchgit {
    url = "https://github.com/awakesecurity/proto3-suite";
    sha256 = "003i6209cp1h82ppb84jgi2ww4zmxm80xd0iz4gw0mh65ilpl6dz";
    rev = "4bfa6275ea9d502d7a30d2bd118ca24903898125";
    fetchSubmodules = true;
  };
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    aeson aeson-pretty attoparsec base base64-bytestring binary
    bytestring cereal containers contravariant deepseq dhall filepath
    foldl ghc hashable insert-ordered-containers large-generics
    large-records lens mtl neat-interpolation parsec parsers pretty
    pretty-show proto3-wire QuickCheck quickcheck-instances safe split
    swagger2 system-filepath text text-short time transformers turtle
    vector
  ];
  executableHaskellDepends = [
    base containers ghc mtl optparse-applicative optparse-generic
    proto3-wire range-set-list system-filepath text turtle
  ];
  testHaskellDepends = [
    aeson attoparsec base base64-bytestring bytestring cereal
    containers deepseq doctest generic-arbitrary ghc hedgehog
    large-generics large-records mtl parsec pretty pretty-show
    proto3-wire QuickCheck record-hasfield swagger2 tasty
    tasty-hedgehog tasty-hunit tasty-quickcheck text text-short
    transformers turtle vector
  ];
  description = "A higher-level API to the proto3-wire library";
  license = lib.licenses.asl20;
}
