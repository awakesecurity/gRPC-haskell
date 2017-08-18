{ mkDerivation, base, bytestring, cereal, containers, deepseq
, doctest, fetchgit, foldl, haskell-src, lens, mtl
, neat-interpolation, optparse-generic, parsec, parsers, pretty
, pretty-show, proto3-wire, QuickCheck, range-set-list, safe
, semigroups, stdenv, system-filepath, tasty, tasty-hunit
, tasty-quickcheck, text, transformers, turtle, vector
}:
mkDerivation {
  pname = "proto3-suite";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/awakesecurity/proto3-suite";
    sha256 = "0pmpnjyw381ksy63nqa9bafzz8gs08k7h3jad7a7hyh1ygxjv19i";
    rev = "88ea97236ac5a03c918badb0961db63f2b0e038c";
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring cereal containers deepseq foldl haskell-src lens
    mtl neat-interpolation parsec parsers pretty pretty-show
    proto3-wire QuickCheck safe semigroups system-filepath text
    transformers turtle vector
  ];
  executableHaskellDepends = [
    base containers optparse-generic proto3-wire range-set-list
    system-filepath text turtle
  ];
  testHaskellDepends = [
    base bytestring cereal doctest pretty-show proto3-wire QuickCheck
    semigroups tasty tasty-hunit tasty-quickcheck text transformers
    turtle vector
  ];
  description = "A low level library for writing out data in the Protocol Buffers wire format";
  license = stdenv.lib.licenses.asl20;
}
