{ pkgs, lib, ... }:

{
  packages = with pkgs; [
    taglib
    zlib
    zlib.dev
  ];

  languages.haskell = {
    enable = true;
    package = pkgs.haskell.compiler.ghc98;
    languageServer = null;
  };
}
