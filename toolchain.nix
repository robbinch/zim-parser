# install with:
# nix-build -j4 -o result-toolchain toolchain.nix
{ nixpkgs ? import <nixpkgs> { } }:

with nixpkgs;
let
  oldDrv = import ./default.nix { inherit nixpkgs; };
in lib.overrideDerivation oldDrv (oldAttrs: {
  phases = [ "installPhase" ];
  name = oldAttrs.name + "-toolchain";
  installPhase = ''
    mkdir -p $out
    echo "$nativeBuildInputs" >> $out/paths
  '';
})
