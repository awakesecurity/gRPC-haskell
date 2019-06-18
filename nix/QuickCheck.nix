{ mkDerivation, base, containers, deepseq, random, stdenv
, template-haskell, tf-random, transformers
}:
mkDerivation {
  pname = "QuickCheck";
  version = "2.10";
  sha256 = "f6f55b798044ad985cce9c38a3d774b32811a0ffdb2066ca9ed45f32b25de7af";
  libraryHaskellDepends = [
    base containers deepseq random template-haskell tf-random
    transformers
  ];
  testHaskellDepends = [ base ];
  homepage = "https://github.com/nick8325/quickcheck";
  description = "Automatic testing of Haskell programs";
  license = stdenv.lib.licenses.bsd3;
}
