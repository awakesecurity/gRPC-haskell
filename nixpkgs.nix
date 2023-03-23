# Given a Git revision hash `<rev>`, you get the new SHA256 by running:
#
# ``` sh
# nix-prefetch-url --unpack 'https://github.com/NixOS/nixpkgs/archive/<rev>.tar.gz'
# ```
#
# The SHA256 will be printed as the last line of stdout.

import (fetchTarball {
  # Branch: release-22.05
  # Date: 2022-07-28
  # grpc.version: 1.46.3
  url = "https://github.com/NixOS/nixpkgs/archive/76e54678eb7fd4d9bfda9b13858a6a8df4501582.tar.gz";
  sha256 = "1zqjzdl9wy17v39nb7z7hsws4ydbqvjsm326x3cgh47m3d24kfhr";
})
