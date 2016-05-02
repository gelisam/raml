module Raml.Classifier where

import Raml.Normalizer


data ClassifiedTree = ClassifiedTree

classify :: NormalizedTree -> ClassifiedTree
classify _ = ClassifiedTree
