#!/bin/bash
set -e
clear
DIR="$(dirname "$0")"

"$DIR/helpers/initialize.sh"

cabal build
"$DIR"/.cabal-sandbox/bin/doctest -i"$DIR"/src $(find "$DIR/src" -name '*.hs')
