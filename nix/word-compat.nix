{ mkDerivation, base, ghc-prim, lib }:
mkDerivation {
  pname = "word-compat";
  version = "0.0.6";
  sha256 = "305c2a9f9aab68cd5a8c9babaa9bc845d296ec5697b7e4ac4a2b368243fb278a";
  libraryHaskellDepends = [ base ghc-prim ];
  testHaskellDepends = [ base ];
  description = "Compatibility shim for the Int/Word internal change in GHC 9.2";
  license = lib.licenses.bsd3;
}
