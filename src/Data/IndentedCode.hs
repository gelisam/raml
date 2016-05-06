module Data.IndentedCode where


type IndentedCode = [CodeChunk]

data CodeChunk
  = Line String
  | Indented IndentedCode
  deriving (Show, Eq)


printIndented :: IndentedCode -> IO ()
printIndented = go ""
  where
    go :: String -> IndentedCode -> IO ()
    go indent = mapM_ (printBlock indent)
    
    printBlock :: String -> CodeChunk -> IO ()
    printBlock indent (Line s) = putStr indent >> putStrLn s
    printBlock indent (Indented ss) = go ("  " ++ indent) ss
