{ mkDerivation, base, containers, fetchgit, generic-deriving, ghc
, large-generics, lib, mtl, newtype, primitive
, record-dot-preprocessor, record-hasfield, syb, tasty, tasty-hunit
, template-haskell, transformers
}:
mkDerivation {
  pname = "large-records";
  version = "0.2.2.0";
  src = fetchgit {
    url = "https://github.com/well-typed/large-records.git";
    sha256 = "0rzssshn6jjdp2l0gx2k60wca17ha62gxq51xyd4rl3vgh2ql7n0";
    rev = "f13dd4514a247dca4a24a7668d702695748f8105";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/large-records; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    base containers ghc large-generics mtl primitive record-hasfield
    syb template-haskell transformers
  ];
  testHaskellDepends = [
    base generic-deriving large-generics mtl newtype
    record-dot-preprocessor record-hasfield tasty tasty-hunit
    template-haskell transformers
  ];
  description = "Efficient compilation for large records, linear in the size of the record";
  license = lib.licenses.bsd3;
}
