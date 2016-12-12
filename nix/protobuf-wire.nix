{ mkDerivation, base, bytestring, cereal, containers, deepseq
, fetchgit, filepath, haskell-src, mtl, parsec, parsers, pipes
, pretty, proto3-wire, QuickCheck, safe, semigroups, stdenv, tasty
, tasty-hunit, tasty-quickcheck, text, transformers, turtle, vector
}:
mkDerivation {
  pname = "protobuf-wire";
  version = "0.1.0.0";
  src = fetchgit {
    url = "git@github.mv.awakenetworks.net:awakenetworks/protobuf-wire.git";
    sha256 = "1d52hd7wq8cfxsp35mmamj0m3mr4705bc76344rhjmsi055r70bc";
    rev = "927c61bbb1002a9278b8a7cbe3968be059f9ff30";
  };
  libraryHaskellDepends = [
    base bytestring cereal containers deepseq filepath haskell-src mtl
    parsec parsers pipes pretty proto3-wire QuickCheck safe semigroups
    text transformers vector
  ];
  testHaskellDepends = [
    base bytestring cereal proto3-wire QuickCheck semigroups tasty
    tasty-hunit tasty-quickcheck text transformers turtle
  ];
  description = "A low level library for writing out data in the Protocol Buffers wire format";
  license = stdenv.lib.licenses.unfree;
}
