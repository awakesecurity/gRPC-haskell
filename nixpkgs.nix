# Given a Git revision hash `<rev>`, you get the new SHA256 by running:
#
# ```bash
# $ nix-prefetch-url "https://github.com/NixOS/nixpkgs/archive/<rev>.tar.gz"
# ```
#
# The SHA256 will be printed as the last line of stdout.

import (builtins.fetchTarball {
    # "nixos-unstable" as on 2024-10-02
    url    = "https://github.com/NixOS/nixpkgs/archive/27e30d177e57d912d614c88c622dcfdb2e6e6515.tar.gz";
    sha256 = "1fvwlz931dvlx4anz7wk3ny33650a292bj4fdj41glckjcfkvzqw";
  })
