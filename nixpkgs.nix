# Given a Git revision hash `<rev>`, you get the new SHA256 by running:
#
# ```bash
# $ nix-prefetch-url "https://github.com/NixOS/nixpkgs/archive/<rev>.tar.gz"
# ```
#
# The SHA256 will be printed as the last line of stdout.

import ./fetch-nixpkgs.nix {
    rev          = "bedbba61380a4da0318de41fcb790c176e1f26d1";
    sha256       = "0z4fgh15nz86kxib9ildmh49v6jim6vgbjyla7jbmgdcl0vd9qsg";
    outputSha256 = "0dxxw2ipa9403nk8lggjsypbr1a9jpb3q4hkjsg89gr5wz26p217";
}
