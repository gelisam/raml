#!/bin/bash
clear
DIR="$(dirname "$0")"
"$DIR"/.cabal-sandbox/bin/doctest "$DIR"/src/*.hs
