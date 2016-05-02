module Raml.Normalizer where

import Raml.Parser


data NormalizedTree = NormalizedTree

normalize :: ParseTree -> NormalizedTree
normalize _ = NormalizedTree
