# Given a Git revision hash `<rev>`, you get the new SHA256 by running:
#
# ```bash
# $ nix-prefetch-url "https://github.com/NixOS/nixpkgs/archive/<rev>.tar.gz"
# ```
#
# The SHA256 will be printed as the last line of stdout.

import ./fetch-nixpkgs.nix {
    rev          = "fa12335f425808f53121713f501f3335878e6901";
    sha256       = "1fjyvjxvymz8yd65ahgm798jp9vdcfy7s58zb5ns2iq2ak0h9j8p";
    outputSha256 = "1qkihrm8xfrh93c7wh1d1x01p7mgv82b2ycpmn9jm5l7976g31vr";
}
