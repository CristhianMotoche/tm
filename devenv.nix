{ pkgs, lib, ... }:

{
  packages = with pkgs; [
    ghc
    cabal-install
    haskell-language-server
    taglib
  ];
}
