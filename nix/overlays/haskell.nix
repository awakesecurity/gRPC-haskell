{ gitignore, ghc }:

final: prev: {
  haskell = prev.haskell // {
    packages = prev.haskell.packages // {
      "${ghc}" = prev.haskell.packages."${ghc}".override (old: {
        overrides = prev.lib.fold prev.lib.composeExtensions (old.overrides or (_: _: { })) [
          (hfinal: hprev: {
            large-records = final.haskell.lib.unmarkBroken hprev.large-records;

            proto3-wire =
              hfinal.callPackage ../packages/proto3-wire.nix { };

            proto3-suite =
              final.haskell.lib.dontCheck
                (hfinal.callPackage ../packages/proto3-suite.nix {});

            grpc-haskell-core =
              final.haskell.lib.buildFromSdist (final.usesGRPC
                (hfinal.callCabal2nix "grpc-haskell-core" (gitignore.lib.gitignoreSource ../../core) {
                    gpr = final.grpc;
                  }
                )
              );

        grpc-haskell-no-tests =
          final.haskell.lib.buildFromSdist (final.usesGRPC
            (final.haskell.lib.dontCheck
              (hfinal.callCabal2nix "grpc-haskell" (gitignore.lib.gitignoreSource ../..) { })
            ));

        grpc-haskell =
          final.usesGRPC
            (final.haskell.lib.overrideCabal
              (final.haskell.lib.buildFromSdist (hfinal.callCabal2nix "grpc-haskell" (gitignore.lib.gitignoreSource ../..) { }))
              (oldDerivation:
                let
                  ghc =
                    hfinal.ghcWithPackages (pkgs: [
                      pkgs.grpc-haskell-no-tests
                      # Include some additional packages in this custom ghc for
                      # running tests in the nix-shell environment.
                      pkgs.tasty-hunit
                      pkgs.tasty-quickcheck
                      pkgs.turtle
                    ]);

                  python = final.python3.withPackages (pkgs: [
                    pkgs.grpcio-tools
                  ]);

                in {
                  configureFlags = (oldDerivation.configureFlags or []) ++ [
                    "--flags=with-examples"
                  ];

                  buildDepends = (oldDerivation.buildDepends or [ ]) ++ [
                    final.makeWrapper

                    # For compile-proto-file
                    hfinal.proto3-suite
                  ];

                  patches =
                    (oldDerivation.patches or [ ]) ++ [ ../../tests/tests.patch ];

                  postPatch = (oldDerivation.postPatch or "") + ''
                    for bin in tests/*.sh; do
                      chmod a+x "$bin"
                    done

                    patchShebangs tests
                    substituteInPlace tests/simple-client.sh \
                      --replace @makeWrapper@ ${final.makeWrapper} \
                      --replace @grpc@ ${final.grpc}
                    substituteInPlace tests/simple-server.sh \
                      --replace @makeWrapper@ ${final.makeWrapper} \
                      --replace @grpc@ ${final.grpc}
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

                  shellHook = (oldDerivation.shellHook or "") + ''
                    # This lets us use our custom ghc and python environments in the shell.
                    export PATH=${ghc}/bin:${python}/bin''${PATH:+:}$PATH
                  '';
                })
            );
          })
        ];
      });
    };
  };

  usesGRPC = haskellPackage:
    # On Linux, LD_LIBRARY_PATH needs to be set for loading
    # grpc-haskell{-,core} code into `ghci` from within `nix-shell`
    # environments.
    #
    # TODO: We might try using pkgsNew.fixDarwinDylibNames (see PR#129)
    # instead of setting DYLD_LIBRARY_PATH, but we might still need them
    # around for `ghci` as on Linux.

    final.haskell.lib.overrideCabal haskellPackage (oldAttributes: {
        preBuild = (oldAttributes.preBuild or "") +
          final.lib.optionalString final.stdenv.isDarwin ''
            export DYLD_LIBRARY_PATH=${final.grpc}/lib''${DYLD_LIBRARY_PATH:+:}$DYLD_LIBRARY_PATH
          '';

        shellHook = (oldAttributes.shellHook or "") +
          final.lib.optionalString final.stdenv.isDarwin ''
            export DYLD_LIBRARY_PATH=${final.grpc}/lib''${DYLD_LIBRARY_PATH:+:}$DYLD_LIBRARY_PATH
          '' +
          final.lib.optionalString final.stdenv.isLinux ''
            export LD_LIBRARY_PATH=${final.grpc}/lib''${LD_LIBRARY_PATH:+:}$LD_LIBRARY_PATH
          '';
      }
    );

  grpc-haskell-dev-shell =
    let
      hsPkgs = final.haskell.packages.${ghc};
    in
      hsPkgs.shellFor {
        name = "grpc-haskell";

        buildInputs = [
          final.haskell-language-server
          final.hlint

          # Give our nix-shell its own cabal so we don't pick up one
          # from the user's environment by accident.
          hsPkgs.cabal-install

          # And likewise for c2hs
          hsPkgs.c2hs
        ];

        packages = pkgs: [pkgs.grpc-haskell];
      };
}
