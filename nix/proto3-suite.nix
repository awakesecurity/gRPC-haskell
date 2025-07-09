{ mkDerivation, aeson, aeson-pretty, attoparsec, attoparsec-aeson
, base, base64-bytestring, binary, bytestring, cereal, containers
, contravariant, deepseq, doctest, fetchgit, filepath, foldl
, generic-arbitrary, ghc-lib-parser, hashable, hedgehog
, insert-ordered-containers, lens, lib, mtl, neat-interpolation
, optparse-applicative, optparse-generic, parsec, parsers, pretty
, pretty-show, proto3-wire, QuickCheck, quickcheck-instances
, record-hasfield, safe, split, swagger2, system-filepath, tasty
, tasty-hedgehog, tasty-hunit, tasty-quickcheck, template-haskell
, text, text-short, time, transformers, turtle, vector
}:
mkDerivation {
  pname = "proto3-suite";
  version = "0.9.3";
  src = fetchgit {
    url = "https://github.com/awakesecurity/proto3-suite";
    sha256 = "19xxvdi28hlxgs9nci5kdsn58kndlwzfjqdzwd2acwwpwccmj0rq";
    rev = "48bf2701622a6b558eb6658cb23fab139efa34db";
    fetchSubmodules = true;
  };
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    aeson aeson-pretty attoparsec attoparsec-aeson base
    base64-bytestring binary bytestring cereal containers contravariant
    deepseq filepath foldl ghc-lib-parser hashable
    insert-ordered-containers lens mtl neat-interpolation parsec
    parsers pretty pretty-show proto3-wire QuickCheck
    quickcheck-instances safe split swagger2 system-filepath
    template-haskell text text-short time transformers turtle vector
  ];
  executableHaskellDepends = [
    base containers ghc-lib-parser mtl optparse-applicative
    optparse-generic proto3-wire system-filepath text turtle
  ];
  testHaskellDepends = [
    aeson attoparsec base base64-bytestring bytestring cereal
    containers deepseq doctest generic-arbitrary ghc-lib-parser
    hedgehog mtl parsec pretty pretty-show proto3-wire QuickCheck
    record-hasfield swagger2 tasty tasty-hedgehog tasty-hunit
    tasty-quickcheck text text-short transformers turtle vector
  ];
  description = "A higher-level API to the proto3-wire library";
  license = lib.licenses.asl20;
}
