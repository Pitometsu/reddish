### Redis client for Haskell

Not intended for production uses.
Use on your own risk.

``` shell
# In case you have no nix for some reason
curl https://nixos.org/nix/install | sh
. ~/.nix-profile/etc/profile.d/nix.sh

# Build & test the library
/usr/bin/env nix-shell --show-trace --pure -Q \
-I nixpkgs=https://github.com/NixOS/nixpkgs/archive/9df2cb074d72ea80ac9fd225b29060c8cf13dd39.tar.gz \
--run "cabal new-test reddish"
```
