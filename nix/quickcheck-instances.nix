{ mkDerivation, array, base, base-compat, bytestring
, case-insensitive, containers, hashable, old-time, QuickCheck
, scientific, stdenv, tagged, text, time, transformers
, transformers-compat, unordered-containers, uuid-types, vector
}:
mkDerivation {
  pname = "quickcheck-instances";
  version = "0.3.15";
  sha256 = "9e0efd0debe1fe390c97d7b3d80d59f3221f2ff4aa1649a1929b4a118156dc0a";
  libraryHaskellDepends = [
    array base base-compat bytestring case-insensitive containers
    hashable old-time QuickCheck scientific tagged text time
    transformers transformers-compat unordered-containers uuid-types
    vector
  ];
  testHaskellDepends = [
    base containers QuickCheck tagged uuid-types
  ];
  homepage = "https://github.com/phadej/qc-instances";
  description = "Common quickcheck instances";
  license = stdenv.lib.licenses.bsd3;
}
