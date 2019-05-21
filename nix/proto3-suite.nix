{ mkDerivation, aeson, aeson-pretty, attoparsec, base
, base64-bytestring, binary, bytestring, cereal, containers
, contravariant, deepseq, doctest, fetchgit, filepath, foldl
, generic-arbitrary, hashable, haskell-src
, insert-ordered-containers, lens, mtl, neat-interpolation
, optparse-applicative, optparse-generic, parsec, parsers, pretty
, pretty-show, proto3-wire, QuickCheck, quickcheck-instances
, range-set-list, safe, semigroups, stdenv, swagger2
, system-filepath, tasty, tasty-hunit, tasty-quickcheck, text
, transformers, turtle, vector
}:
mkDerivation {
  pname = "proto3-suite";
  version = "0.4.0.0";
  src = fetchgit {
    url = "https://github.com/awakesecurity/proto3-suite";
    sha256 = "0wyw8601k6rm8wjz8mf6pl43ccm2nmikrd6ddr7h3qj0aj55qjyc";
    rev = "f5ca2bee361d518de5c60b9d05d0f54c5d2f22af";
    fetchSubmodules = true;
  };
  jailbreak = true;
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    aeson aeson-pretty attoparsec base base64-bytestring binary
    bytestring cereal containers contravariant deepseq filepath foldl
    hashable haskell-src insert-ordered-containers lens mtl
    neat-interpolation parsec parsers pretty pretty-show proto3-wire
    QuickCheck quickcheck-instances safe semigroups swagger2
    system-filepath text transformers turtle vector
  ];
  executableHaskellDepends = [
    base containers mtl optparse-applicative optparse-generic
    proto3-wire range-set-list system-filepath text turtle
  ];
  testHaskellDepends = [
    aeson attoparsec base base64-bytestring bytestring cereal
    containers deepseq doctest generic-arbitrary mtl pretty-show
    proto3-wire QuickCheck semigroups swagger2 tasty tasty-hunit
    tasty-quickcheck text transformers turtle vector
  ];
  description = "A low level library for writing out data in the Protocol Buffers wire format";
  license = stdenv.lib.licenses.asl20;
}
