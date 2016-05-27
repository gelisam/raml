{-# LANGUAGE DeriveGeneric, OverloadedStrings, RecordWildCards #-}
import Options.Generic

import Data.Yaml.Ordered.MyExtra
import Raml.Parser
import Raml.Normalizer
import Raml.Classifier
import Raml.Analyzer
import Raml.Generator
import Raml.Simplifier
import Raml.PrettyPrinter
import Data.IndentedCode


data CompilationOptions = CompilationOptions
  { ramlFile :: FilePath
  } deriving (Generic, Show, Eq)

instance ParseRecord CompilationOptions


compile :: CompilationOptions -> IO ()
compile (CompilationOptions {..}) = do
    r <- prettyPrint
     <$> simplify
     <$> generate
     <$> analyze
     <$> classify
     <$> normalize
     <$> parse
     <$> readYaml ramlFile
    printIndented r

main :: IO ()
main = getRecord "Generate Scala types corresponding to the given RAML types." >>= compile
