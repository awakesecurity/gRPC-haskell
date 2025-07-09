# Given a Git revision hash `<rev>`, you get the new SHA256 by running:
#
# ```bash
# $ nix-prefetch-url "https://github.com/NixOS/nixpkgs/archive/<rev>.tar.gz"
# ```
#
# The SHA256 will be printed as the last line of stdout.

import (builtins.fetchTarball {
    # "nixos-unstable" as on 2025-04-18
    url    = "https://github.com/NixOS/nixpkgs/archive/b024ced1aac25639f8ca8fdfc2f8c4fbd66c48ef.tar.gz";
    sha256 = "09dahi81cn02gnzsc8a00n945dxc18656ar0ffx5vgxjj1nhgsvy";
  })
