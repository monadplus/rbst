{ fp ? ./nixpkgs.json }:

let
  nixpkgsJson = builtins.fromJSON (builtins.readFile fp);
  nixpkgsFunc = import (builtins.fetchTarball (with nixpkgsJson; {
    url = "${url}/archive/${rev}.tar.gz";
    inherit sha256;
  }));
in
  nixpkgsFunc {}