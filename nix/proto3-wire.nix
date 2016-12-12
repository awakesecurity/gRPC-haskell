{ mkDerivation, base, bytestring, cereal, containers, deepseq
, fetchgit, QuickCheck, safe, stdenv, tasty, tasty-hunit
, tasty-quickcheck, text
}:
mkDerivation {
  pname = "proto3-wire";
  version = "1.0.0";
  src = fetchgit {
    url = "https://github.com/awakenetworks/proto3-wire.git";
    sha256 = "1v1jsgsdrhaz3ddbil09yqimnps3svgqbjvdk7hil4irpgqkfs98";
    rev = "1b88bf24aad15db1f59a00d201d609fa308157f7";
  };
  libraryHaskellDepends = [
    base bytestring cereal containers deepseq QuickCheck safe text
  ];
  testHaskellDepends = [
    base bytestring cereal QuickCheck tasty tasty-hunit
    tasty-quickcheck text
  ];
  description = "A low-level implementation of the Protocol Buffers (version 3) wire format";
  license = stdenv.lib.licenses.asl20;
}
