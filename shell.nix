{ nixpkgs ? import <nixpkgs> {}, compiler ? "lts-3_8" }:
(import ./default.nix { inherit nixpkgs compiler; }).env
