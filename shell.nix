{ ghc ? null }:
let 
  pkgs = import ./release.nix { inherit ghc; };
in pkgs.grpc-haskell.env.overrideAttrs (self: {
  buildInputs = self.buildInputs ++ [
    pkgs.grpc
  ];
})