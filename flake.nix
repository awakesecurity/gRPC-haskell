# If you would like to test and build changes quickly using `cabal`, run:
#
#     $ nix develop
#     [nix-shell]$ cabal configure --enable-tests && cabal test
#
# This will open up a Nix shell where all of your Haskell tools will work like
# normal, except that all dependencies (including C libraries) are managed by
# Nix.  The only thing that won't work is running tests inside this shell
# (although you can still build them).  Fixing the test suite requires
# extensive patching of the test scripts (see `postPatch` below)
#
# Note that this will compile the library once without tests using Nix.  This
# is due to the fact that `grpc-haskell`'s test suite cannot test code
# generation without the library being built at least once.
#
# If you want to build and test this repository using `nix`, you can run the
# following command:
#
#     $ nix build .#grpc-haskell
#
# ... but this is not recommended for normal development because this will
# rebuild the repository from scratch every time, which is extremely slow.  Only
# do this if you want to exactly reproduce our continuous integration build.
#
# By default, Nix will pick a version for each one of your Haskell dependencies.
# If you would like to select a different version then, run:
#
#     $ cabal2nix cabal://${package-name}-${version} > nix/${package-name}.nix
#
# ... and then add this line below in the Haskell package overrides section in
# the haskell overlay:
#
#     ${package-name} =
#       haskellPackagesNew.callPackage ../${package-name}.nix { };
#
# ... replacing `${package-name}` with the name of the package that you would
# like to upgrade and `${version}` with the version you want to upgrade to.
#
# You can also add private Git dependencies in the same way, except supplying
# the `git` URL to clone:
#
#     $ cabal2nix <your private git url>/${package-name}.git > ./nix/${package-name}.nix
#
# ...but also be sure to supply `fetchgit = pkgs.fetchgitPrivate` in the
# `haskellPackagesNew.callPackage` invocation for your private package.
#
# Note that `cabal2nix` also takes an optional `--revision` flag if you want to
# pick a revision other than the latest to depend on.
#
# If you want to test a local source checkout of a dependency, then run:
#
#     $ cabal2nix path/to/dependency/repo > nix/${package-name}.nix

{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/24.05";
    flake-utils.url = "github:numtide/flake-utils";
    gitignore = {
      url = "github:hercules-ci/gitignore.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, flake-utils, gitignore }:
    flake-utils.lib.eachSystem ["x86_64-linux" "x86_64-darwin" "aarch64-darwin"] (system:
      let
        ghc = "ghc94";

        haskellOverlay = import nix/overlays/haskell.nix {
          inherit gitignore ghc;
        };

        pkgs = import nixpkgs {
          inherit system;
          overlays = [ haskellOverlay ];
        };

        haskellPackages = pkgs.haskell.packages.${ghc};
      in {
        packages = {
          default = haskellPackages.grpc-haskell;
          inherit (haskellPackages)
            grpc-haskell
            grpc-haskell-no-tests
            grpc-haskell-core;
        };
        devShells.default = pkgs.grpc-haskell-dev-shell;
      });
}

