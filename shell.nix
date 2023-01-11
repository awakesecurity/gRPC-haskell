let 
  pkgs = import ./release.nix;
in pkgs.grpc-haskell.env.overrideAttrs (self: {
  buildInputs = self.buildInputs ++ [
    pkgs.grpc
  ];
})