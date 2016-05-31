{-# LANGUAGE DeriveGeneric, OverloadedStrings, RecordWildCards #-}
import qualified Filesystem.Path.CurrentOS as Path
import Options.Generic

import Data.Yaml.Ordered.MyExtra
import Raml.Parser
import Raml.Normalizer
import Raml.Classifier
import Raml.Analyzer
import Language.Scala.Generator
import Language.Scala.IncludeTracker
import Language.Scala.Name
import Language.Scala.PrettyPrinter
import Language.Scala.Simplifier
import Data.IndentedCode


data CompilationOptions = CompilationOptions
  { packageName :: Maybe PackageName
  , ramlFile :: Path.FilePath
  } deriving (Generic, Show, Eq)

instance ParseRecord CompilationOptions


compile :: CompilationOptions -> IO ()
compile (CompilationOptions {..}) = do
    r <- flattenLayout
     <$> runIncludeTracker packageName
     <$> return
     <$> prettyPrint
     <$> simplify
     <$> generate
     <$> analyze
     <$> classify
     <$> normalize
     <$> parse
     <$> readYaml (Path.encodeString ramlFile)
    printBlock r

main :: IO ()
main = getRecord "Generate Scala types corresponding to the given RAML types." >>= compile
