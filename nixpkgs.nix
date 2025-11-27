# Given a Git revision hash `<rev>`, you get the new SHA256 by running:
#
# ```bash
# $ nix-prefetch-url "https://github.com/NixOS/nixpkgs/archive/<rev>.tar.gz"
# ```
#
# The SHA256 will be printed as the last line of stdout.

import (builtins.fetchTarball {
    # "nixos-unstable" as on 2025-11-27
    url    = "https://github.com/NixOS/nixpkgs/archive/5ae3b07d8d6527c42f17c876e404993199144b6a.tar.gz";
    sha256 = "sha256-6eeL1YPcY1MV3DDStIDIdy/zZCDKgHdkCmsrLJFiZf0=";
  })
