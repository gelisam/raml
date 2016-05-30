{-# LANGUAGE DeriveGeneric, OverloadedStrings, RecordWildCards #-}
import qualified Filesystem.Path.CurrentOS as Path
import Options.Generic
import Text.Printf

import Data.Maybe.MyExtra
import Data.Yaml.Ordered.MyExtra
import Raml.Parser
import Raml.Normalizer
import Raml.Classifier
import Raml.Analyzer
import Language.Scala.Generator
import Language.Scala.Simplifier
import Language.Scala.PrettyPrinter
import Data.IndentedCode


addPackage :: String -> [[IndentedCode]] -> [[IndentedCode]]
addPackage name = ([[Line package_line]]:)
  where
    package_line :: String
    package_line = printf "package %s" name


data CompilationOptions = CompilationOptions
  { packageName :: Maybe String
  , ramlFile :: Path.FilePath
  } deriving (Generic, Show, Eq)

instance ParseRecord CompilationOptions


compile :: CompilationOptions -> IO ()
compile (CompilationOptions {..}) = do
    r <- (addPackage `maybeAp` packageName)
     <$> prettyPrint
     <$> simplify
     <$> generate
     <$> analyze
     <$> classify
     <$> normalize
     <$> parse
     <$> readYaml (Path.encodeString ramlFile)
    printIndented r

main :: IO ()
main = getRecord "Generate Scala types corresponding to the given RAML types." >>= compile
