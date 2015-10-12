{ nixpkgs ? import <nixpkgs> {}, compiler ? "lts-3_8" }:
nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./zim-parser.nix { }
