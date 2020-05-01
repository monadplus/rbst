{ nixpkgs     ? import ./nix/nixpkgs.nix {}
, compiler    ? "ghc883"
, doBenchmark ? true
}:
let
  inherit (nixpkgs) pkgs;
  env = (import ./default.nix { inherit nixpkgs compiler doBenchmark; }).env;
in
  env.overrideAttrs (oldAttrs: {
    buildInputs = with pkgs.haskellPackages; oldAttrs.buildInputs ++ [
      cabal-install cabal2nix ghcid
    ];
  })
