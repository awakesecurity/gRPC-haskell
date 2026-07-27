# Given a Git revision hash `<rev>`, you get the new SHA256 by running:
#
# ```bash
# $ nix-prefetch-url "https://github.com/NixOS/nixpkgs/archive/<rev>.tar.gz"
# ```
#
# The SHA256 will be printed as the last line of stdout.

import (fetchTarball {
    # "nixos-unstable" as on 2026-07-26
    url    = "https://github.com/NixOS/nixpkgs/archive/624af665418d3c65d544145b4d34ad696439570e.tar.gz";
    sha256 = "sha256-m0pDuRJG7EDo9ri+4Ksu83VsI+PlxNC9lNBfydejce4=";
  })
