{ mkDerivation, async, base, bytestring, c2hs, clock, containers
, grpc, managed, pipes, proto3-wire, protobuf-wire, random, safe
, sorted-list, stdenv, stm, tasty, tasty-hunit, tasty-quickcheck
, text, time, transformers, turtle, unix, vector
}:
mkDerivation {
  pname = "grpc-haskell";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    async base bytestring clock containers managed pipes proto3-wire
    protobuf-wire safe sorted-list stm tasty tasty-hunit transformers
    vector
  ];
  librarySystemDepends = [ grpc ];
  libraryToolDepends = [ c2hs ];
  executableHaskellDepends = [
    async base bytestring containers proto3-wire protobuf-wire random
    text transformers
  ];
  testHaskellDepends = [
    async base bytestring clock containers managed pipes protobuf-wire
    safe tasty tasty-hunit tasty-quickcheck text time transformers
    turtle unix
  ];
  homepage = "http://github.com/aloiscochard/grpc-haskell";
  description = "Haskell implementation of gRPC layered on shared C library";
  license = stdenv.lib.licenses.asl20;
}
