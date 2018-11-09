{ mkDerivation, base, bytestring, Only, optparse-applicative
, semigroups, stdenv, system-filepath, text, time, transformers
, void
}:
mkDerivation {
  pname = "optparse-generic";
  version = "1.3.0";
  sha256 = "80929958606e4a73672b570ba1a23493fbf46268666d14ab5af53623301c398f";
  libraryHaskellDepends = [
    base bytestring Only optparse-applicative semigroups
    system-filepath text time transformers void
  ];
  description = "Auto-generate a command-line parser for your datatype";
  license = stdenv.lib.licenses.bsd3;
}
