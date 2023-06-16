# Given a Git revision hash `<rev>`, you get the new SHA256 by running:
#
# ```bash
# $ nix-prefetch-url "https://github.com/NixOS/nixpkgs/archive/<rev>.tar.gz"
# ```
#
# The SHA256 will be printed as the last line of stdout.

import (builtins.fetchTarball {
    # nixos-21.11 as on 2023-06-15
    url    = "https://github.com/NixOS/nixpkgs/archive/eabc38219184cc3e04a974fe31857d8e0eac098d.tar.gz";
    sha256 = "04ffwp2gzq0hhz7siskw6qh9ys8ragp7285vi1zh8xjksxn1msc5";
  })
