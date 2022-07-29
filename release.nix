# If you would like to test and build changes quickly using `cabal`, run:
#
#     $ nix-shell
#     [nix-shell]$ cabal configure --enable-tests && cabal build && cabal test
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
#     $ nix-build --attr grpc-haskell release.nix
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
# ... and then add this line below in the Haskell package overrides section:
#
#     ${package-name} =
#       hsPkgsSelf.callPackage ./nix/${package-name}.nix { };
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
# `hsPkgsSelf.callPackage` invocation for your private package.
#
# Note that `cabal2nix` also takes an optional `--revision` flag if you want to
# pick a revision other than the latest to depend on.
#
# If you want to test a local source checkout of a dependency, then run:
#
#     $ cabal2nix path/to/dependency/repo > nix/${package-name}.nix
#
# Finally, if you want to add grpc-haskell to your own package set, you can
# setup the overlay with:
#
#     grpc-nixpkgs = import path/to/gRPC-haskell/nixpkgs.nix;
#     grpc-overlay = (import path/to/gRPC-haskell/release.nix).overlay;
#     # optionally use the same nixpkgs source
#     pkgs = grpc-nixpkgs { overlays = [ grpc-overlay ]; };
#
# ... and use the extend function to setup haskell package override:
#
#     # see https://github.com/NixOS/nixpkgs/issues/25887
#     haskellPackages = pkgs.haskellPackages.extend (self: super: {
#       your-package = self.callCabal2nix "your-package" ./. { };
#     };);

let
  # Taking specific Python version.
  #
  # N.B. Mind that some of the dependencies of gRPC Python packages
  # (“grpcio-status” and “grpcio-tools”) do not support Python 2 anymore:
  # “error: mox-0.7.8 not supported for interpreter python2.7”.
  # So do not use “python” as a value since it defaults to Python 2.
  pythonVer = "python3";

  overlay = pkgsSelf: pkgsSuper: {

    # “grpc-haskell” is made for specific version of gRPC library.
    # So this override makes sure it would still work against another nixpkgs
    # pin where gRPC can have different version.
    grpc = pkgsSelf.callPackage nix/grpc-pin/grpc.nix {
      # grpc builds with c++14 so abseil must also be built that way
      abseil-cpp = pkgsSelf.abseil-cpp_202111.override {
        cxxStandard = "14";
      };
    };

    # As recommended for “grpc” derivation also overriding
    # “grpcio-status” and “grpcio-tools” to the same version.
    ${pythonVer} = pkgsSuper.${pythonVer}.override {
      packageOverrides = pySelf: pySuper: {
        grpcio-status = pySelf.callPackage nix/grpc-pin/grpcio-status.nix {};
        grpcio-tools = pySelf.callPackage nix/grpc-pin/grpcio-tools.nix {};
      };
    };

    haskellPackages = (hsPkgsOverridden pkgsSuper).extend (hsSelf: hsSuper: {

      grpc-haskell-core =
        pkgsSelf.lib.pipe (
          hsSelf.callCabal2nix "grpc-haskell-core" ./core {
            gpr = pkgsSelf.grpc;
          }
        ) [
          pkgsSelf.usesGRPC
          pkgsSelf.haskell.lib.buildFromSdist
        ];

      grpc-haskell-no-tests =
        pkgsSelf.lib.pipe (hsSelf.callCabal2nix "grpc-haskell" ./. { }) [
          pkgsSelf.haskell.lib.dontCheck
          pkgsSelf.usesGRPC
          pkgsSelf.haskell.lib.buildFromSdist
        ];

      grpc-haskell =
        let
          ghc =
            hsSelf.ghcWithPackages (pkgs: [
              pkgs.grpc-haskell-no-tests
              # Include some additional packages in this custom ghc for
              # running tests in the nix-shell environment.
              pkgs.pipes
              pkgs.tasty-hunit
              pkgs.tasty-quickcheck
              pkgs.turtle
            ]);

          python = pkgsSelf.${pythonVer}.withPackages (pkgs: [
            pkgs.grpcio-tools
          ]);

          override = old: {
            configureFlags = (old.configureFlags or []) ++ [
              "--flags=with-examples"
            ];

            buildDepends = (old.buildDepends or [ ]) ++ [
              pkgsSelf.makeWrapper

              # Give our nix-shell its own cabal so we don't pick up one
              # from the user's environment by accident.
              hsSelf.cabal-install

              # And likewise for c2hs
              hsSelf.c2hs
            ];

            patches =
              (old.patches or [ ]) ++ [ ./tests/tests.patch ];

            postPatch = (old.postPatch or "") + ''
              patchShebangs tests
              substituteInPlace tests/simple-client.sh \
                --replace @makeWrapper@ ${pkgsSelf.makeWrapper} \
                --replace @grpc@ ${pkgsSelf.grpc}
              substituteInPlace tests/simple-server.sh \
                --replace @makeWrapper@ ${pkgsSelf.makeWrapper} \
                --replace @grpc@ ${pkgsSelf.grpc}
              wrapProgram tests/protoc.sh \
                --prefix PATH : ${python}/bin
              wrapProgram tests/test-client.sh \
                --prefix PATH : ${python}/bin
              wrapProgram tests/test-server.sh \
                --prefix PATH : ${python}/bin
              wrapProgram tests/simple-client.sh \
                --prefix PATH : ${ghc}/bin
              wrapProgram tests/simple-server.sh \
                --prefix PATH : ${ghc}/bin
            '';

            shellHook = (old.shellHook or "") + ''
              # This lets us use our custom ghc and python environments in the shell.
              export PATH=${ghc}/bin:${python}/bin''${PATH:+:}$PATH
            '';
          };
        in
          pkgsSelf.lib.pipe (hsSelf.callCabal2nix "grpc-haskell" ./. { }) [
            pkgsSelf.haskell.lib.buildFromSdist
            (pkgsSelf.lib.flip pkgsSelf.haskell.lib.overrideCabal override)
            pkgsSelf.usesGRPC
          ];

    });

    test-grpc-haskell =
      pkgsSelf.mkShell {
        nativeBuildInputs = [
          (pkgsSelf.haskellPackages.ghcWithPackages (pkgs: [
            pkgs.grpc-haskell
          ]))
        ];
      };

    usesGRPC = haskellPackage:
      # On Linux, LD_LIBRARY_PATH needs to be set for loading
      # grpc-haskell{-,core} code into `ghci` from within `nix-shell`
      # environments.
      #
      # TODO: We might try using pkgsSelf.fixDarwinDylibNames (see PR#129)
      # instead of setting DYLD_LIBRARY_PATH, but we might still need them
      # around for `ghci` as on Linux.

      pkgsSelf.haskell.lib.overrideCabal haskellPackage (oldAttributes: {
        preBuild = (oldAttributes.preBuild or "") +
          pkgsSelf.lib.optionalString pkgsSelf.stdenv.isDarwin ''
            export DYLD_LIBRARY_PATH=${pkgsSelf.grpc}/lib''${DYLD_LIBRARY_PATH:+:}$DYLD_LIBRARY_PATH
          '';

        shellHook = (oldAttributes.shellHook or "") +
          pkgsSelf.lib.optionalString pkgsSelf.stdenv.isDarwin ''
            export DYLD_LIBRARY_PATH=${pkgsSelf.grpc}/lib''${DYLD_LIBRARY_PATH:+:}$DYLD_LIBRARY_PATH
          '' +
          pkgsSelf.lib.optionalString pkgsSelf.stdenv.isLinux ''
            export LD_LIBRARY_PATH=${pkgsSelf.grpc}/lib''${LD_LIBRARY_PATH:+:}$LD_LIBRARY_PATH
          '';
      });
  };

  hsAddPatch = pkgs: patchFile:
    pkgs.lib.flip pkgs.haskell.lib.overrideCabal (old: {
      patches = (old.patches or [ ]) ++ [ patchFile ];
    });

  hsMarkUnbroken = pkgs:
    pkgs.lib.flip pkgs.haskell.lib.overrideCabal (old: { broken = false; });

  # Overrides for Haskell packages this library depends on
  hsPkgsOverridden = pkgs:
    pkgs.haskellPackages.extend (self: super: {
      data-diverse =
        pkgs.lib.pipe (self.callPackage nix/data-diverse.nix {}) [
          # Patch for GHC 9.x support
          (hsAddPatch pkgs (pkgs.fetchpatch {
            url = "https://github.com/louispan/data-diverse/commit/4033c90c44dab5824f76d64b7128bb6dea2b5dc7.patch";
            sha256 = "sha256-d6bC1Z7uCLtYF3FXGzo3XNdRPQgeAUjL1RW1Tiv7MnM=";
          }))

          # The patch above makes it not to be broken anymore
          (hsMarkUnbroken pkgs)
        ];

      proto3-wire =
        pkgs.lib.pipe (self.callPackage nix/proto3-wire.nix {}) [
          (hsAddPatch pkgs nix/proto3-wire.patch)
        ];

      proto3-suite =
        pkgs.lib.pipe (self.callPackage nix/proto3-suite.nix {}) [
          (hsAddPatch pkgs nix/proto3-suite.patch)
          pkgs.haskell.lib.dontCheck # 4 out of 74 tests failed
        ];
    });

  overlays = [ overlay ];

  config  = { };

   nixpkgs = import ./nixpkgs.nix;
   linuxPkgs = nixpkgs { inherit config overlays; system = "x86_64-linux" ; };
  darwinPkgs = nixpkgs { inherit config overlays; system = "x86_64-darwin"; };
        pkgs = nixpkgs { inherit config overlays; };

  shell = pkgs.haskellPackages.shellFor {
    name = "gRPC-haskell-shell";
    withHoogle = true;

    packages = p: [
      p.grpc-haskell-core
      p.grpc-haskell
    ];

    buildInputs = [
      pkgs.cabal-install
      pkgs.grpc
    ];
  };

  # Stack build using Nix requires also “gmp” and “zlib”
  stack-env =
    pkgs.haskellPackages.grpc-haskell.env.overrideAttrs (old: {
      propagatedBuildInputs = (old.propagatedBuildInputs or [ ]) ++ [
        pkgs.gmp
        pkgs.zlib
        pkgs.grpc
      ];
    });

in
  {
    grpc-haskell-core-linux    =  linuxPkgs.haskellPackages.grpc-haskell-core;
    grpc-haskell-core-darwin   = darwinPkgs.haskellPackages.grpc-haskell-core;
    grpc-haskell-core          =       pkgs.haskellPackages.grpc-haskell-core;

    grpc-haskell-linux         =  linuxPkgs.haskellPackages.grpc-haskell;
    grpc-haskell-darwin        = darwinPkgs.haskellPackages.grpc-haskell;
    grpc-haskell               =       pkgs.haskellPackages.grpc-haskell;
    grpc-haskell-no-tests      =       pkgs.haskellPackages.grpc-haskell-no-tests;

    grpc-linux                 =  linuxPkgs.grpc;
    grpc-darwin                = darwinPkgs.grpc;

    grpc                       =       pkgs.grpc;

    inherit pkgs config overlay shell stack-env;
    inherit (pkgs) test-grpc-haskell;

    inherit hsPkgsOverridden; # Function :: nixpkgs -> new haskellPackages
    inherit (hsPkgsOverridden (nixpkgs {})) data-diverse proto3-wire proto3-suite;
  }
