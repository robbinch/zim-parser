# install with:
# nix-build -j4 -o result-toolchain toolchain.nix
{ pkgs ? import <nixpkgs> { } }:

with pkgs;
let
  oldDrv = import ./default.nix { inherit pkgs; };
in lib.overrideDerivation oldDrv (oldAttrs: {
  phases = [ "installPhase" ];
  name = "zim-parser-toolchain";
  installPhase = ''
    mkdir -p $out
    echo "$nativeBuildInputs" >> $out/paths
  '';
})
