Running the tests
-----------------

In order to run the tests, you will need to have the `grpcio`, `gevent`, and
`grpcio-tools` python packages installed. You can install them using
`pip`. It is recommended that you use a python virtualenv to do this.

```
$ virtualenv path/to/virtualenv # to create a virtualenv
$ . path/to/virtualenv/bin/activate # to use an existing virtualenv
$ pip install grpcio-tools gevent
$ pip install grpcio # Need to install grpcio-tools first to avoid a versioning problem
```

Building GRPC
-------------

In order to compile this project, and anything which depends on it, you will need a working installation
of the GRPC C core libraries. This library currently uses the 0.15 version range. If you are on OS X, you can install it with homebrew:

```
brew tap grpc/grpc
brew install grpc
```

Alternatively, you can build gRPC from source by checking out an appropriate revision
of the repository, and installing as follows:

```sh
git clone https://github.com/grpc/grpc.git
git checkout release-0_15_1
cd grpc
git submodule update --init
make
sudo make install
```

Alternatively, using Nix, pass the following expression to `nix-build` and point Stack to the build products in the Nix store:

```nix
let pkgs = import <nixpkgs> {};
in  pkgs.stdenv.mkDerivation rec
    {   name = "grpc";
        src = pkgs.fetchgit
        { url    = "https://github.com/grpc/grpc.git";
          rev    = "674b30373e2d6a1e26425952805179f8d52a8c00";
          sha256 = "05vj48w4h7bn6xyf1wyg2l6psl38h4yz6j1cl0yd2p5h7f5hb3s7";
        };
        preInstall = "export prefix";
        buildInputs =
        [   pkgs.darwin.cctools
            pkgs.autoconf
            pkgs.automake
            pkgs.libtool
            pkgs.which
            pkgs.zlib

            pkgs.openssl
        ];
    }
```

Using the Library
-----------------

You must compile with `-threaded`, because we rely on being able to execute Haskell while blocking on foreign calls to the gRPC library. If not using code generation, the recommended place to start is in the `Network.GRPC.HighLevel.Server.Unregistered` module, where `serverLoop` provides a handler loop.
