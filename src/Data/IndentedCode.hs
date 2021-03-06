module Data.IndentedCode where

import Data.List
import Data.Yaml.Ordered (ToJSON(..))

import Data.Yaml.Ordered.MyExtra


type IndentedCode = [CodeChunk]

data CodeChunk
  = Line String
  | Indented IndentedCode
  deriving (Show, Eq)


instance ToJSON CodeChunk where
  toJSON (Line x) = YamlString x
  toJSON (Indented xs) = toJSON xs


unindentedLine :: String -> IndentedCode
unindentedLine s = [Line s]

unindentedLines :: [String] -> IndentedCode
unindentedLines = foldMap unindentedLine


printIndented :: IndentedCode -> IO ()
printIndented = go ""
  where
    go :: String -> IndentedCode -> IO ()
    go indent = mapM_ (printBlock indent)
    
    printBlock :: String -> CodeChunk -> IO ()
    printBlock indent (Line s) = putStr indent >> putStrLn s
    printBlock indent (Indented ss) = go ("  " ++ indent) ss


type GroupedCode = [[IndentedCode]]

singleBlank :: IndentedCode
singleBlank = [Line ""]

doubleBlank :: IndentedCode
doubleBlank = [Line "", Line ""]

layoutGroupedCode :: GroupedCode -> IndentedCode
layoutGroupedCode = intercalate doubleBlank
                  . map (intercalate singleBlank . filter (not . null))
                  . filter (not . null)
