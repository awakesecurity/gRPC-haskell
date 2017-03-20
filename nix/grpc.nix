{ pkgs, stdenv, lib, fetchgit, autoconf, automake, libtool, which, zlib, openssl
}:

stdenv.mkDerivation rec {
  name    = "grpc-${version}";
  version = "1.1.4-${lib.strings.substring 0 7 rev}";
  rev     = "ed7d06af3eef1c27f10328c73b3ae3ab10d72b10";
  src = fetchgit {
    inherit rev;
    url    = "https://github.com/grpc/grpc.git";
    sha256 = "1qh4fm3v11qa5b17br5bbdn2c6qgv2q5dpy4gmviz9cjyqxgjvxw";
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
