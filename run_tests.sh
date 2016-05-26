#!/bin/bash
set -e
clear
DIR="$(dirname "$0")"

if [ ! -d "$DIR/.cabal-sandbox" ]; then
  cabal sandbox init
  cabal install doctest
  cabal install
fi

cabal build
"$DIR"/.cabal-sandbox/bin/doctest -i"$DIR"/src $(find "$DIR/src" -name '*.hs')

cabal install
.cabal-sandbox/bin/raml-to-scala tests/sample.in | diff - tests/sample.scala
