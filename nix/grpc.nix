{ pkgs, stdenv, lib, fetchgit, autoconf, automake, libtool, which, zlib, openssl
}:

stdenv.mkDerivation rec {
  name    = "grpc-${version}";
  version = "1.1.0-${lib.strings.substring 0 7 rev}";
  rev     = "686375b5ac2aeb3696958c4959235171aae323a5";
  src = fetchgit {
    inherit rev;
    url    = "https://github.com/grpc/grpc.git";
    sha256 = "00ckprs2lm2mq0mg0sy3iq3n9fn8887vzv6gh3r3wlg6nvcm0vdw";
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
