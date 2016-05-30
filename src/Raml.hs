module Raml
  ( Regexp
  , TypeName
  , PropertyName
  , BranchName
  , BuiltinType(..)
  , StringFieldProps(..)
  , Field(..)
  , BranchProps(..)
  , NamedSumProps(..)
  , NamedProductProps(..)
  , Raml.Analyzer.TypeProps(..)
  , AnalyzedTree(..)
  , Raml
  , readRaml
  ) where

import Data.Yaml.Ordered.MyExtra
import Raml.Common
import Raml.Parser
import Raml.Normalizer
import Raml.Classifier
import Raml.Analyzer


type Raml = AnalyzedTree

readRaml :: FilePath -> IO Raml
readRaml path = analyze
            <$> classify
            <$> normalize
            <$> parse
            <$> readYaml path
