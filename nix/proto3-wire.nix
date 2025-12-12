{ mkDerivation, base, bytestring, cereal, containers, criterion
, deepseq, doctest, fetchgit, hashable, lib, parameterized
, primitive, QuickCheck, random, safe, tasty, tasty-hunit
, tasty-quickcheck, template-haskell, text, text-short
, transformers, unordered-containers, vector, word-compat
}:
mkDerivation {
  pname = "proto3-wire";
  version = "1.4.6";
  src = fetchgit {
    url = "https://github.com/awakesecurity/proto3-wire";
    sha256 = "1nidk369y7gwjbcg4c1wg4q7kjbskyfj2xxbhs0qk16c7nvmay7n";
    rev = "fcc53d9935b64b6d8aaf65c8cef17f4bbed56867";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base bytestring cereal containers deepseq hashable parameterized
    primitive QuickCheck safe template-haskell text text-short
    transformers unordered-containers vector word-compat
  ];
  testHaskellDepends = [
    base bytestring cereal containers doctest QuickCheck tasty
    tasty-hunit tasty-quickcheck text text-short transformers vector
  ];
  benchmarkHaskellDepends = [ base bytestring criterion random ];
  description = "A low-level implementation of the Protocol Buffers (version 3) wire format";
  license = lib.licenses.asl20;
}
