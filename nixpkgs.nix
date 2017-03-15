let
  # NOTE: This is the only non-deterministic part of our system since we need a
  # a starting point in order to be able to fetch the pinned `nixpkgs`.  From
  # that point forward our build is deterministic and pinned
  #
  # We only use this for the `fetchFromGitHub` utility so as long as that
  # remains stable then we shouldn't have migration issues.
  inherit (import <nixpkgs> { }) fetchFromGitHub;

  # In order to update `nixpkgs.json` to a specific revision, run:
  #
  # ```bash
  # $ nix-prefetch-git https://github.com/NixOS/nixpkgs.git "${REVISION}" > nixpkgs.json
  # ```
  nixpkgs = builtins.fromJSON (builtins.readFile ./nixpkgs.json);
in
  fetchFromGitHub {
    owner = "NixOS";
    repo  = "nixpkgs";
    inherit (nixpkgs) rev sha256;
  }
