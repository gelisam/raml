module Raml.Analyzer where

import Raml.Classifier


data AnalyzedTree = AnalyzedTree

analyze :: ClassifiedTree -> AnalyzedTree
analyze _ = AnalyzedTree
