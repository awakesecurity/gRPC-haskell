# Given a Git revision hash `<rev>`, you get the new SHA256 by running:
#
# ```bash
# $ nix-prefetch-url "https://github.com/NixOS/nixpkgs/archive/<rev>.tar.gz"
# ```
#
# The SHA256 will be printed as the last line of stdout.

import ./fetch-nixpkgs.nix {
  rev    = "1849e695b00a54cda86cb75202240d949c10c7ce";
  sha256 = "1riv7n11rqbfdnikr2wm263fcppzh0760kqhwn5gscl89qmliw2y";
}
