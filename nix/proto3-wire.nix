{ mkDerivation, base, bytestring, cereal, containers, deepseq
, doctest, fetchgit, hashable, QuickCheck, safe, stdenv, tasty
, tasty-hunit, tasty-quickcheck, text, unordered-containers
}:
mkDerivation {
  pname = "proto3-wire";
  version = "1.0.0";
  src = fetchgit {
    url = "git@github.com:awakesecurity/proto3-wire.git";
    sha256 = "1ps1lijjbwzps2axr94r4k9c6cw9c9s5cy2izf2867hgavxrjs6k";
    rev = "4f715b29031566878333d81ff0845be766a5d381";
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
