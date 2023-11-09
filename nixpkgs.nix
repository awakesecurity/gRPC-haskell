# Given a Git revision hash `<rev>`, you get the new SHA256 by running:
#
# ```bash
# $ nix-prefetch-url "https://github.com/NixOS/nixpkgs/archive/<rev>.tar.gz"
# ```
#
# The SHA256 will be printed as the last line of stdout.

import (builtins.fetchTarball {
    # "master" as on 2022-04-22
    url    = "https://github.com/NixOS/nixpkgs/archive/ed3cc9672ad507ca4d00e15b35f3d24edd1dff6c.tar.gz";
    sha256 = "1bn55f20kqpdcfz0gsn9j6cyw617izrnwb4yw33bgqhyxlabb4q0";
  })
