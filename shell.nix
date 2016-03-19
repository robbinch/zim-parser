{ nixpkgs ? import <nixpkgs> {}, compiler ? "lts-5_8" }:
(import ./default.nix { inherit nixpkgs compiler; }).env
