{ mkDerivation, base, containers, deepseq, hashable, lib, tasty
, tasty-quickcheck
}:
mkDerivation {
  pname = "range-set-list";
  version = "0.1.3.1";
  sha256 = "12e8d9cb99a2847da32934ed7f44a5acedaa59d8fa19eff0f46aa77921460c55";
  revision = "5";
  editedCabalFile = "17a016lbs5p94bclgzqkyld4vhp4rnv4rg8xk4qhqyagggrvq9k7";
  libraryHaskellDepends = [ base containers deepseq hashable ];
  testHaskellDepends = [
    base containers deepseq hashable tasty tasty-quickcheck
  ];
  homepage = "https://github.com/phadej/range-set-list#readme";
  description = "Memory efficient sets with ranges of elements";
  license = lib.licenses.mit;
}
