#!/usr/bin/env nix-shell
set -euo pipefail
IFS=$'\n\t'

cabal v1-sdist
cabal upload --publish dist/rbst-0.0.0.0.tar.gz
cabal haddock --haddock-html-location='https://hackage.haskell.org/package/$pkg-$version/docs' --haddock-hyperlink-source --haddock-quickjump --haddock-for-hackage
