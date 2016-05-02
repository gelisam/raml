module Raml.Generator where

import Raml.Analyzer


data GeneratedTree = GeneratedTree

generate :: AnalyzedTree -> GeneratedTree
generate _ = GeneratedTree
