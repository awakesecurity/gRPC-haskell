{ pkgs, stdenv, lib, fetchgit, autoconf, automake, libtool, which, zlib, openssl
}:

stdenv.mkDerivation rec {
  name    = "grpc-${version}";
  version = "0.15-${lib.strings.substring 0 7 rev}";
  rev     = "03efbd34ce64615f58007eae667b375accc6c8e6";
  src = fetchgit {
    inherit rev;
    url    = "https://github.com/grpc/grpc.git";
    sha256 = "0a48swsip09bd0yk80gl9r7pny9dal3byyd22bdz4fcvydna43m0";
  };
  preInstall  = "export prefix";
  buildInputs =
    (if stdenv.isDarwin then [ pkgs.darwin.cctools ] else []) ++ [
    autoconf
    automake
    libtool
    which
    zlib
    openssl
  ];
}
