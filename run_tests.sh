#!/bin/bash
clear
DIR="$(dirname "$0")"
cabal build
"$DIR"/.cabal-sandbox/bin/doctest -i"$DIR"/src "$DIR"/src/*.hs
