{ mkDerivation, base, bytestring, cereal, containers, deepseq
, fetchgit, filepath, haskell-src, mtl, parsec, parsers, pipes
, pretty, proto3-wire, QuickCheck, safe, semigroups, stdenv, tasty
, tasty-hunit, tasty-quickcheck, text, transformers, turtle, vector
}:
mkDerivation {
  pname = "proto3-suite";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/awakenetworks/proto3-suite.git";
    sha256 = "0wvr54x3v6g9yb30nd5g9gfnn7j8is2hl8drwmmiyz0n45zlz0qh";
    rev = "827af4bf644d05e88b5d95dfe40febe7b43fab16";
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
  license = stdenv.lib.licenses.asl20;
}
