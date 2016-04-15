#!/bin/bash
set -e
clear
DIR="$(dirname "$0")"
cabal build
"$DIR"/.cabal-sandbox/bin/doctest -i"$DIR"/src $(find "$DIR/src" -name '*.hs')
