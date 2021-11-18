#!/usr/bin/env nix-shell
set -euo pipefail
IFS=$'\n\t'

cabal sdist
cabal upload "$1" -u ArnauAbella # Upload to candidates
cabal upload "$1" --documentation --publish -u ArnauAbella # Publish
cabal haddock --enable-documentation --haddock-for-hackage # Publish documentation
