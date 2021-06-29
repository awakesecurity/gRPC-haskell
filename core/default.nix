{ mkDerivation, async, base, bytestring, c2hs, clock, containers
, gpr, grpc, managed, pipes, proto3-suite, QuickCheck, safe
, sorted-list, stdenv, stm, tasty, tasty-hunit, tasty-quickcheck
, text, time, transformers, turtle, unix
}:
mkDerivation {
  pname = "grpc-haskell-core";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring clock containers managed sorted-list stm
    transformers
  ];
  librarySystemDepends = [ gpr grpc ];
  libraryToolDepends = [ c2hs ];
  testHaskellDepends = [
    async base bytestring clock containers managed pipes proto3-suite
    QuickCheck safe tasty tasty-hunit tasty-quickcheck text time
    transformers turtle unix
  ];
  homepage = "https://github.com/awakenetworks/gRPC-haskell";
  description = "Haskell implementation of gRPC layered on shared C library";
  license = stdenv.lib.licenses.asl20;
}
