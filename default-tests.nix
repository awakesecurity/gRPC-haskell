{ grpc-haskell-no-tests
, ghc
, python
}:

attrs@
{ mkDerivation, async, base, bytestring, c2hs, clock, containers
, grpc, managed, pipes, proto3-wire, protobuf-wire, random, safe
, sorted-list, stdenv, stm, tasty, tasty-hunit, tasty-quickcheck
, text, time, transformers, turtle, unix, vector
}:

let
  mkDerivation' = oldAttrs: mkDerivation (oldAttrs // {
    patches = [ tests/tests.patch ];

    postPatch = ''
      substituteInPlace tests/simple-server.sh --replace @ghc@ ${ghc} --replace @shell@ ${stdenv.shell}
      substituteInPlace tests/protoc.sh --replace @python@ ${python} --replace @shell@ ${stdenv.shell}
    '';

    testHaskellDepends = oldAttrs.testHaskellDepends ++ [
      ghc grpc-haskell-no-tests
    ];

    doCheck = true;
  });

in import ./default.nix (attrs // { mkDerivation = mkDerivation'; })
