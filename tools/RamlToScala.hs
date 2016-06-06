{-# LANGUAGE DeriveGeneric, OverloadedStrings, RecordWildCards #-}
import qualified Filesystem.Path.CurrentOS as Path
import Options.Generic

import Data.IndentedCode
import Language.Scala.Annotator
import qualified Language.Scala.Annotator.Pattern as Pattern
import Language.Scala.Converter
import Language.Scala.Generator
import Language.Scala.IncludeTracker
import Language.Scala.Name
import Language.Scala.PrettyPrinter
import Language.Scala.Simplifier
import Raml


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
     <$> annotate Pattern.topLevelAnnotator Pattern.branchAnnotator
     <$> convert
     <$> readRaml (Path.encodeString ramlFile)
    printBlock r

main :: IO ()
main = getRecord "Generate Scala types corresponding to the given RAML types." >>= compile
