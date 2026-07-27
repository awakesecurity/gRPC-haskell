{ ghc ? null }:
(import ../release.nix { inherit ghc; }).grpc-haskell-core.env
