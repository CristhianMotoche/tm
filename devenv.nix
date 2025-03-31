{ pkgs, lib, ... }:

{
  packages = with pkgs; [
    taglib
    zlib
    zlib.dev
    apple-sdk_11
  ];

  languages.haskell = {
    enable = true;
    package = pkgs.haskell.compiler.ghc98;
    languageServer = pkgs.haskell-language-server.override {
      supportedGhcVersions = [ "98" ];
    };
  };
}
