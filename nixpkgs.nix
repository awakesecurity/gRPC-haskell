# Given a Git revision hash `<rev>`, you get the new SHA256 by running:
#
# ```bash
# $ nix-prefetch-url --unpack "https://github.com/NixOS/nixpkgs/archive/<rev>.tar.gz"
# ```
#
# The SHA256 will be printed as the last line of stdout.

import ./fetch-nixpkgs.nix {
   rev    = "74286ec9e76be7cd00c4247b9acb430c4bd9f1ce";
   sha256 = "13ydgpzl5nix4gc358iy9zjd5nrrpbpwpxmfhis4aai2zmkja3ak";
}
