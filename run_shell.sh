#!/usr/bin/env bash
nix-shell --packages 'haskellPackages.ghcWithHoogle (pkgs: with pkgs; [ QuickCheck mwc-random ])'
