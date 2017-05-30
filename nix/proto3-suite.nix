{ mkDerivation, base, bytestring, cereal, containers, deepseq
, fetchgit, filepath, haskell-src, mtl, optparse-generic, parsec
, parsers, pretty, pretty-show, proto3-wire, QuickCheck, safe
, semigroups, stdenv, system-filepath, tasty, tasty-hunit
, tasty-quickcheck, text, transformers, turtle, vector
}:
mkDerivation {
  pname = "proto3-suite";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/awakenetworks/proto3-suite";
    sha256 = "0hvspgrfjwk5dbsf4ka02dbbjs8w91q2w52wmyc6jhlnc9b1r3cv";
    rev = "10f592cdd5349e33d4924b066d4f6e27ada5e5c5";
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring cereal containers deepseq filepath haskell-src mtl
    parsec parsers pretty pretty-show proto3-wire QuickCheck safe
    semigroups text transformers vector
  ];
  executableHaskellDepends = [
    base optparse-generic system-filepath turtle
  ];
  testHaskellDepends = [
    base bytestring cereal pretty-show proto3-wire QuickCheck
    semigroups tasty tasty-hunit tasty-quickcheck text transformers
    turtle vector
  ];
  description = "A low level library for writing out data in the Protocol Buffers wire format";
  license = stdenv.lib.licenses.asl20;
}
