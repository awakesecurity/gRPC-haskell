# Given a Git revision hash `<rev>`, you get the new SHA256 by running:
#
# ```bash
# $ nix-prefetch-url "https://github.com/NixOS/nixpkgs/archive/<rev>.tar.gz"
# ```
#
# The SHA256 will be printed as the last line of stdout.

import (builtins.fetchTarball {
    # "nixos-unstable" as on 2024-12-02
    url    = "https://github.com/NixOS/nixpkgs/archive/ac35b104800bff9028425fec3b6e8a41de2bbfff.tar.gz";
    sha256 = "1fbj7shlmviilmgz5z2gp59j6xwgdr01jfh75qhixx06kib4305p";
  })
