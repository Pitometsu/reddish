{ dev ? true }:
with import <nixpkgs> {}; mkShell {
  src = builtins.toString ./.;
  buildInputs = [
    redis
    wget gnupg coreutils procps-ng
    which less curl ripgrep tree
    gitMinimal openssh man-db ] ++ (with haskellPackages; [
      ghc cabal-install ghci ghcid
      ghcide hasktags haskell-language-server
      fourmolu hoogle hlint
      mtl network-simple parsec vinyl ]);
  GIT_SSL_CAINFO = "${cacert}/etc/ssl/certs/ca-bundle.crt";
  LANG = "C.UTF-8";
  LC_ALL = "C.UTF-8";
  PAGER="less -R";
  shellHook = ''
    ulimit -Sv 8388608
    ulimit -v 10485760
    nohup redis-server &> /dev/null 2>&1 &
    redis_down() { pkill redis-server; }
    trap redis_down EXIT
  ''; }
