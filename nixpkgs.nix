# Given a Git revision hash `<rev>`, you get the new SHA256 by running:
#
# ```bash
# $ nix-prefetch-url "https://github.com/NixOS/nixpkgs/archive/<rev>.tar.gz"
# ```
#
# The SHA256 will be printed as the last line of stdout.

import (builtins.fetchTarball {
    # "nixos-unstable" as on 2026-01-06
    url    = "https://github.com/NixOS/nixpkgs/archive/9f0c42f8bc7151b8e7e5840fb3bd454ad850d8c5.tar.gz";
    sha256 = "sha256-UWYqmD7JFBEDBHWYcqE6s6c77pWdcU/i+bwD6XxMb8A=";
  })
