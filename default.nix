{ pkgs ? import <nixpkgs> {}, compiler ? "lts-3_8" }:
let

  profiledPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: {
      mkDerivation = args: super.mkDerivation ( args // {
        enableLibraryProfiling = true;
      });
    };
  };

  # ghc = pkgs.haskell.packages.${compiler}.ghcWithPackages (ps: with ps; [
  ghc = profiledPackages.ghcWithPackages (ps: with ps; [
    lzma-conduit
    conduit-extra
    binary-conduit
    hspec
  ]);
in
pkgs.stdenv.mkDerivation {
  name = "zim-parser";
  buildInputs = [ ghc ];
  shellHook = "eval $(egrep ^export ${ghc}/bin/ghc)";
}
