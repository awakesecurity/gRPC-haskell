{ mkDerivation, base, bytestring, cereal, containers, deepseq
, fetchgit, QuickCheck, safe, stdenv, tasty, tasty-hunit
, tasty-quickcheck, text
}:
mkDerivation {
  pname = "proto3-wire";
  version = "1.0.0";
  src = fetchgit {
    url = "https://github.com/awakenetworks/proto3-wire.git";
    sha256 = "10z1sirmiz29r2nx5dza1y1p3kp83wsq80pz4msxqmaykpyh5iaa";
    rev = "62b50ea460847dde5bc8e63d2f93360d9bfcae9d";
  };
  libraryHaskellDepends = [
    base bytestring cereal containers deepseq QuickCheck safe text
  ];
  testHaskellDepends = [
    base bytestring cereal QuickCheck tasty tasty-hunit
    tasty-quickcheck text
  ];
  description = "A low-level implementation of the Protocol Buffers (version 3) wire format";
  license = stdenv.lib.licenses.asl20;
}
