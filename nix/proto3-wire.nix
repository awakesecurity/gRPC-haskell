{ mkDerivation, base, bytestring, cereal, containers, deepseq
, doctest, ghc-prim, hashable, lib, parameterized, primitive
, QuickCheck, safe, tasty, tasty-hunit, tasty-quickcheck, text
, transformers, unordered-containers, vector
}:
mkDerivation {
  pname = "proto3-wire";
  version = "1.2.2";
  sha256 = "8d409536a89a0187f0576711966d2ef45d43acab7b6a3a1c5ee12f6d01adbfb9";
  libraryHaskellDepends = [
    base bytestring cereal containers deepseq ghc-prim hashable
    parameterized primitive QuickCheck safe text transformers
    unordered-containers vector
  ];
  testHaskellDepends = [
    base bytestring cereal doctest QuickCheck tasty tasty-hunit
    tasty-quickcheck text transformers vector
  ];
  description = "A low-level implementation of the Protocol Buffers (version 3) wire format";
  license = lib.licenses.asl20;
}
