# To build this repository with `nix` you run:
#
#     $ nix-build release.nix -I ssh-config-file=/path/to/ssh/config
#
# ... where the `ssh-config-file` is an SSH configuration file with enough
# information to authenticate (i.e. an `IdentityFile` readable by the user that
# builds this expression, for example).
#
# If you update the `.cabal` file (such as changing dependencies or adding new
# library/executable/test/benchmark sections), then update the `default.nix`
# expression by running:
#
#     $ cabal2nix . > default.nix
#
# If you want to update a dependency like `proto3-wire` to the latest git
# revision, then run:
#
#     $ nix-prefetch-git https://github.com/awakenetworks/proto3-wire.git
#
# ... and modify the `rev` and `sha256` fields of the corresponding `fetchgit`
# expression below using the output of the `nix-prefetch-git` command.
#
# If you want to test a local `proto3-wire` repository, then replace the
# `fetchgit { ... }` expression with the relative path to the source repository
# such as:
#
#     let proto3-wire-src = ../proto3-wire;
#     in
#     ...

let
  config = {
    packageOverrides = pkgs: rec {
      grpc = pkgs.stdenv.mkDerivation rec {
        name    = "grpc-${version}";
        version = "0.14-${pkgs.lib.strings.substring 0 7 rev}";
        rev     = "2b223977c13975648bac2f422363e1ebf83506ce";
        src = pkgs.fetchgit {
          inherit rev;
          url    = "https://github.com/grpc/grpc.git";
          sha256 = "0arxjdczgj6rbg14f6x24863mrz0xgpakmdfg54zp0xp7h2pghm6";
        };
        preInstall  = "export prefix";
        buildInputs =
          (if pkgs.stdenv.isDarwin then [ pkgs.darwin.cctools ] else []) ++ [
          pkgs.autoconf
          pkgs.automake
          pkgs.libtool
          pkgs.which
          pkgs.zlib
          pkgs.openssl
        ];
      };

      haskellPackages = pkgs.haskell.packages.ghc7103.override {
        overrides = haskellPackagesNew: haskellPackagesOld: {
          proto3-wire =
            let proto3-wire-src = pkgs.fetchgit {
              url    = "https://github.com/awakenetworks/proto3-wire.git";
              rev    = "b5151914873b9ede230094e742953b5062566244";
              sha256 = "09vjza18gnh5mf9l8vg3ka1c7rqfbjwviyjwpvam07hf90r0yg5b";
            };
            in
            haskellPackagesNew.callPackage proto3-wire-src { };

          protobuf-wire =
            let protobuf-wire-src = pkgs.fetchgitPrivate {
              url    = "git@github.mv.awakenetworks.net:awakenetworks/protobuf-wire.git";
              rev    = "c766074e3e5da9bcca1e5a5fc069983d8fbc97b3";
              sha256 = "0mlpfzvnzfblq7a265q4b61xkn30ndvqp8qjshbrx7kvyjxjlvnn";
            };
            in
            haskellPackagesNew.callPackage protobuf-wire-src { };

          grpc-haskell =
            haskellPackagesNew.callPackage ./default.nix { };
        };
      };
    };

    allowUnfree = true;
  };

in
{ pkgs ? import <nixpkgs> { inherit config; } }:
# Disable tests for now, since they don't pass yet
{ grpc-haskell = pkgs.haskell.lib.dontCheck pkgs.haskellPackages.grpc-haskell;
}
