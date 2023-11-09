{ mkDerivation, base, bytestring, cereal, containers, criterion
, deepseq, doctest, fetchgit, ghc-prim, hashable, lib
, parameterized, primitive, QuickCheck, random, safe, tasty
, tasty-hunit, tasty-quickcheck, template-haskell, text, text-short
, transformers, unordered-containers, vector, word-compat
}:
mkDerivation {
  pname = "proto3-wire";
  version = "1.4.1";
  src = fetchgit {
    url = "https://github.com/awakesecurity/proto3-wire.git";
    sha256 = "sha256-g15oAYmhfv8DVgmQTZkcCgOYRFkxl2PQi30qXXXhBLs=";
    rev = "ee6ca644eef86cc5f31da85fb48e10b20ab0e1a1";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base bytestring cereal containers deepseq ghc-prim hashable
    parameterized primitive QuickCheck safe template-haskell text
    text-short transformers unordered-containers vector word-compat
  ];
  testHaskellDepends = [
    base bytestring cereal doctest QuickCheck tasty tasty-hunit
    tasty-quickcheck text text-short transformers vector
  ];
  benchmarkHaskellDepends = [ base bytestring criterion random ];
  description = "A low-level implementation of the Protocol Buffers (version 3) wire format";
  license = lib.licenses.asl20;
}
