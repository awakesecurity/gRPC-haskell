{ pkgs, stdenv, lib, fetchgit, autoconf, automake, libtool, which, zlib, openssl
}:

stdenv.mkDerivation rec {
  name    = "grpc-${version}";
  version = "1.0.1-${lib.strings.substring 0 7 rev}";
  rev     = "6040b471bcd1d6bb05b25c126b6545180a1d3528";
  src = fetchgit {
    inherit rev;
    url    = "https://github.com/grpc/grpc.git";
    sha256 = "1kx6jkx2dnnfnjfyc50ravfk7mfdszj988vndrlzs1zkd6627k4z";
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
