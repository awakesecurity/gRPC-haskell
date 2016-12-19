Building and testing
--------------------

You will need to have GRPC installed already. See the "Installing GRPC" section
below for more information.

`nix-build release.nix -A grpc-haskell` will build and test the whole thing and
put the completed package into the nix store.

`nix-shell` can be used to give you a development environment where you can use
the `cabal` and `stack test` toolchains for development and testing iteration.

```bash
$ nix-shell release-nix -A grpc-haskell.env
[nix-shell]$ cabal configure --with-gcc=clang --enable-tests && cabal build && cabal test
```

```bash
$ nix-shell release-nix -A grpc-haskell.env
[nix-shell]$ stack build --fast && stack test --fast
```

Note that, for `stack`, the `nix-shell` environment only needed to run the
tests, because it uses some custom python tooling (for grpc interop
testing). You should still be able to `stack build` without using the
`nix-shell` environment at all. See the section below on installing GRPC for use
by `stack`.

NB: You can also instruct `stack` to run the tests inside the `nix-shell`
environment directly, via `stack --nix test --fast`. However, this will
frequently rebuild the custom ghc that is used by the `nix` tooling so is not
recommended for development iterations.

Finally, since `stack` does not use `nix` for any Haskell dependencies,
repository references for dependent packages such as `protobuf-wire` must be
updated both in `nix/<pkg>.nix` AND in the `stack.yaml`.

Installing GRPC for `stack`
---------------------------

If you want to use the `stack` tooling, you will need a working installation of
the GRPC C core libraries.

On MacOS, because
of [this](https://github.com/commercialhaskell/stack/issues/1161), dependencies
on the nix-built `grpc` don't work properly, so the library needs to be
installed somewhere the linker will pick it up without `DYLD_LIBRARY_PATH` set.

We suggest that use `brew` to do this:

```
brew tap grpc/grpc
brew edit grpc
...
brew install grpc
```

Make sure you select a release version that is reasonably close to our grpc
dependency, e.g.:

```
url "https://github.com/grpc/grpc/archive/release-0_15_0.tar.gz"
sha256 "d02235dff278869e94cb0dcb31cfea935693c6f87bd73f43d44147185e6becdd"
```

or

```
url "https://github.com/grpc/grpc/archive/v1.0.1.tar.gz"
sha256 "efad782944da13d362aab9b81f001b7b8b1458794751de818e9848c47acd4b32"
```

Using the Library
-----------------

You must compile with `-threaded`, because we rely on being able to execute
Haskell while blocking on foreign calls to the gRPC library. If not using code
generation, the recommended place to start is in the
`Network.GRPC.HighLevel.Server.Unregistered` module, where `serverLoop` provides
a handler loop.
