{ pkgs ? import <nixpkgs> {}, hpkgs ? pkgs.haskellPackages, mkDerivation ? hpkgs.mkDerivation }:
let loquacious = hpkgs.callPackage ../loquacious { inherit mkDerivation; };
in hpkgs.callCabal2nix "err" ./. { inherit loquacious; }
