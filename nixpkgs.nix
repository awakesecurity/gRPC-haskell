# Given a Git revision hash `<rev>`, you get the new SHA256 by running:
#
# ```bash
# $ nix-prefetch-url "https://github.com/NixOS/nixpkgs/archive/<rev>.tar.gz"
# ```
#
# The SHA256 will be printed as the last line of stdout.

import ./fetch-nixpkgs.nix {
     rev          = "dd9f73e7d34486b09b966738ace161e621a0480b";
     sha256       = "0000000000000000000000000000000000000000000000000000000000000000";
     outputSha256 = "0s674386v5b24a9fia26439gw9wsyhif85k2nzpxkp61293v3n3h";
}
