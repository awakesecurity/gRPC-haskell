{ mkDerivation, base, bytestring, cereal, containers, criterion
, deepseq, doctest, fetchgit, hashable, lib, parameterized
, primitive, QuickCheck, random, safe, tasty, tasty-hunit
, tasty-quickcheck, template-haskell, text, text-short
, transformers, unordered-containers, vector, word-compat
}:
mkDerivation {
  pname = "proto3-wire";
  version = "1.4.5";
  src = fetchgit {
    url = "https://github.com/awakesecurity/proto3-wire";
    sha256 = "0nb8xy723jhybrfsyfcgpczgbv80hcndprc45h3zq9hliam07qqv";
    rev = "d4376fb6f1c1ac03ee8ec5c5793700ca6508ea70";
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
