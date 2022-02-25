{ mkDerivation, aeson, aeson-pretty, attoparsec, base
, base64-bytestring, binary, bytestring, cereal, containers
, contravariant, deepseq, doctest, fetchgit, filepath, foldl
, generic-arbitrary, hashable, haskell-src
, insert-ordered-containers, lens, lib, mtl, neat-interpolation
, optparse-applicative, optparse-generic, parsec, parsers, pretty
, pretty-show, proto3-wire, QuickCheck, quickcheck-instances
, range-set-list, safe, swagger2, system-filepath, tasty
, tasty-hunit, tasty-quickcheck, text, time, transformers, turtle
, vector
}:
mkDerivation {
  pname = "proto3-suite";
  version = "0.4.3";
  src = fetchgit {
    url = "https://github.com/awakesecurity/proto3-suite.git";
    sha256 = "0bjqczi6wddxv0n7qmfbrr19ajgq66xdkxx8vfcgbmv8ygma3vlw";
    rev = "7af7d76dcf9cc71ddada3aa4a38abf46f65550ca";
    fetchSubmodules = true;
  };
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    aeson aeson-pretty attoparsec base base64-bytestring binary
    bytestring cereal containers contravariant deepseq filepath foldl
    hashable haskell-src insert-ordered-containers lens mtl
    neat-interpolation parsec parsers pretty pretty-show proto3-wire
    QuickCheck quickcheck-instances safe swagger2 system-filepath text
    time transformers turtle vector
  ];
  executableHaskellDepends = [
    base containers mtl optparse-applicative optparse-generic
    proto3-wire range-set-list system-filepath text turtle
  ];
  testHaskellDepends = [
    aeson attoparsec base base64-bytestring bytestring cereal
    containers deepseq doctest generic-arbitrary mtl pretty-show
    proto3-wire QuickCheck swagger2 tasty tasty-hunit tasty-quickcheck
    text transformers turtle vector
  ];
  description = "A higher-level API to the proto3-wire library";
  license = lib.licenses.asl20;
}
