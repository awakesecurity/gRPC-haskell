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
    sha256 = "1imhijvbliax8n2sz74jwwyb0m6zy8v1xkmixlm6vchwicn2ka3k";
    rev = "d9d0282978a785385c47642a9a46037e4263a87d";
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
