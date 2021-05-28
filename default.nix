{ pkgs ? import <nixpkgs> {}
, hpkgs ? pkgs.haskellPackages
, mkDerivation ? hpkgs.mkDerivation
, loquacious ? hpkgs.loquacious or import ../loquacious { inherit pkgs hpkgs mkDerivation; }
}: hpkgs.callCabal2nix "err" (pkgs.lib.cleanSource ./.) { inherit mkDerivation loquacious; }
