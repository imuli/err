{ pkgs ? import <nixpkgs> {}
, hpkgs ? pkgs.haskellPackages
, mkDerivation ? hpkgs.mkDerivation
, loquate ? hpkgs.loquate or import ../loquate { inherit pkgs hpkgs mkDerivation; }
}: hpkgs.callCabal2nix "err" (pkgs.lib.cleanSource ./.) { inherit mkDerivation loquate; }
