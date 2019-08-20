{ mkDerivation, base, bytestring, cereal, containers, deepseq
, doctest, fetchgit, hashable, QuickCheck, safe, stdenv, tasty
, tasty-hunit, tasty-quickcheck, text, unordered-containers
}:
mkDerivation {
  pname = "proto3-wire";
  version = "1.1.0";
  src = fetchgit {
    url = "https://github.com/awakesecurity/proto3-wire.git";
    sha256 = "16l1rnnygwk1b2sb3l6klhr6ad0wvry204icxnc81c6rbzbk6rqc";
    rev = "4f355bbac895d577d8a28f567ab4380f042ccc24";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base bytestring cereal containers deepseq hashable QuickCheck safe
    text unordered-containers
  ];
  testHaskellDepends = [
    base bytestring cereal doctest QuickCheck tasty tasty-hunit
    tasty-quickcheck text
  ];
  description = "A low-level implementation of the Protocol Buffers (version 3) wire format";
  license = stdenv.lib.licenses.asl20;
}
