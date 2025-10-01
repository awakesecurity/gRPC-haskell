# Given a Git revision hash `<rev>`, you get the new SHA256 by running:
#
# ```bash
# $ nix-prefetch-url "https://github.com/NixOS/nixpkgs/archive/<rev>.tar.gz"
# ```
#
# The SHA256 will be printed as the last line of stdout.

import (builtins.fetchTarball {
    # "nixos-unstable" as on 2025-09-30
    url    = "https://github.com/NixOS/nixpkgs/archive/e9f00bd893984bc8ce46c895c3bf7cac95331127.tar.gz";
    sha256 = "0s2mhbrgzxlgkg2yxb0q0hpk8lby1a7w67dxvfmaz4gsmc0bnvfj";
  })
