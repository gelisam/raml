#!/bin/bash
set -e
DIR="$(dirname "$0")"

if [ ! -d "$DIR/../.cabal-sandbox" ]; then
  cabal sandbox init
  cabal install doctest
  cabal install
fi
