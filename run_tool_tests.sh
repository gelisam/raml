#!/bin/bash
set -e
clear
DIR="$(dirname "$0")"

"$DIR/helpers/initialize.sh"

cabal install
.cabal-sandbox/bin/raml-to-scala --packageName=com.keatext.gelineau.samuel.hello --ramlFile tests/sample.in | diff - tests/sample.scala
