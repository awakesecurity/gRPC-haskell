{ mkDerivation, aeson, base, deepseq, fetchgit, generic-deriving
, generics-sop, lib, microlens, mtl, primitive, QuickCheck
, sop-core, tasty, tasty-hunit, tasty-quickcheck
}:
mkDerivation {
  pname = "large-generics";
  version = "0.2.0.0";
  src = fetchgit {
    url = "https://github.com/well-typed/large-records.git";
    sha256 = "0rzssshn6jjdp2l0gx2k60wca17ha62gxq51xyd4rl3vgh2ql7n0";
    rev = "f13dd4514a247dca4a24a7668d702695748f8105";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/large-generics; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    aeson base deepseq generics-sop primitive sop-core
  ];
  testHaskellDepends = [
    aeson base generic-deriving generics-sop microlens mtl QuickCheck
    sop-core tasty tasty-hunit tasty-quickcheck
  ];
  description = "Generic programming API for large-records and large-anon";
  license = lib.licenses.bsd3;
}
