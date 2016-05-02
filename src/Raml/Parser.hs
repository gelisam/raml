module Raml.Parser where

import Data.Yaml as Yaml


data ParseTree = ParseTree

parse :: Yaml.Value -> ParseTree
parse _ = ParseTree
