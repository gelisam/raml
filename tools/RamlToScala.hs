{-# LANGUAGE RecordWildCards #-}
import System.Environment

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
  } deriving (Show, Eq)

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

raml_to_scala :: [String] -> IO ()
raml_to_scala [fileName] = compile (CompilationOptions fileName)
raml_to_scala _ = do
    putStrLn "usage:"
    putStrLn "  raml-to-scala file.raml > file.scala"
    -- TODO: more information please!

main :: IO ()
main = getArgs >>= raml_to_scala
