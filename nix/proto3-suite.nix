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
    sha256 = "11zrkpnr5i3axw8m0dxi0camffdmcz0a0dnd6ivsw8s187a1k07c";
    rev = "683f100ca3200594f3dc4a723e52cc5b5e237053";
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
