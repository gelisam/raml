{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.IndentedCode where

import Data.List
import Data.Yaml.Ordered (ToJSON(..))

import Data.List.MyExtra
import Data.Yaml.Ordered.MyExtra


-- This is a Line,
--   this is an
--   Indented block,
-- this is a second line,
--   this is a second indented block
--   consisting of four lines,
--   
--   one of which is blank,
-- and this is a third line.
-- 
-- The above lines and indented blocks are grouped in a single CodeBlock.
-- These two lines are part of a second CodeBlock.
-- 
-- 
-- The last two blocks together are clumped in a CodeGroup.
-- This block
-- 
-- and this block
-- 
-- and this block
-- are all clumped into a second CodeGroup.
-- 
-- 
-- This third group completes the example.
-- Together, all three groups are clumped in a single CodeLayout.


-- $setup
-- >>> :{
--   let codeLayout =
--         CodeLayout
--         [ CodeGroup
--           [ CodeBlock
--             [ Line "This is a Line,"
--             , Indented $ CodeBlock
--               [ Line "this is an"
--               , Line "Indented block,"
--               ]
--             , Line "this is a second line,"
--             , Indented $ CodeBlock
--               [ Line "this is a second indented block"
--               , Line "consisting of four lines,"
--               , Line ""
--               , Line "one of which is blank,"
--               ]
--             , Line "and this is a third line."
--             ]
--           , multiLineBlock
--             [ "The above lines and indented blocks are grouped in a single CodeBlock."
--             , "These two lines are part of a second CodeBlock."
--             ]
--           ]
--         , multiBlockGroup
--           [ multiLineBlock
--             [ "The last two blocks together are clumped in a CodeGroup."
--             , "This block"
--             ]
--           , singleLineBlock "and this block"
--           , multiLineBlock
--             [ "and this block"
--             , "are all clumped into a second CodeGroup."
--             ]
--           ]
--         , multiLineGroup
--           [ "This third group completes the example."
--           , "Together, all three groups are clumped in a single CodeLayout."
--           ]
--         ]
-- :}


data CodeChunk
  = Line String
  | Indented CodeBlock
  deriving (Show, Eq)

newtype CodeBlock = CodeBlock
  { runCodeBlock :: [CodeChunk]
  } deriving (Show, Eq, Monoid, ToJSON)

newtype CodeGroup = CodeGroup
  { runCodeGroup :: [CodeBlock]
  } deriving (Show, Eq, Monoid, ToJSON)

newtype CodeLayout = CodeLayout
  { runCodeLayout :: [CodeGroup]
  } deriving (Show, Eq, Monoid, ToJSON)


instance ToJSON CodeChunk where
  toJSON (Line x) = YamlString x
  toJSON (Indented xs) = toJSON xs


singleLineBlock :: String -> CodeBlock
singleLineBlock s = CodeBlock [Line s]

multiLineBlock :: [String] -> CodeBlock
multiLineBlock = CodeBlock
               . foldMap (runCodeBlock . singleLineBlock)


singleBlockGroup :: CodeBlock -> CodeGroup
singleBlockGroup = CodeGroup . return

multiBlockGroup :: [CodeBlock] -> CodeGroup
multiBlockGroup = CodeGroup

singleLineGroup :: String -> CodeGroup
singleLineGroup = singleBlockGroup . singleLineBlock

multiLineGroup :: [String] -> CodeGroup
multiLineGroup = singleBlockGroup . multiLineBlock


singleGroupLayout :: CodeGroup -> CodeLayout
singleGroupLayout = CodeLayout . return

multiGroupLayout :: [CodeGroup] -> CodeLayout
multiGroupLayout = CodeLayout

singleBlockLayout :: CodeBlock -> CodeLayout
singleBlockLayout = singleGroupLayout . singleBlockGroup

multiBlockLayout :: [CodeBlock] -> CodeLayout
multiBlockLayout = singleGroupLayout . multiBlockGroup

singleLineLayout :: String -> CodeLayout
singleLineLayout = singleGroupLayout . singleLineGroup

multiLineLayout :: [String] -> CodeLayout
multiLineLayout = singleGroupLayout . multiLineGroup


flattenGroup :: CodeGroup -> CodeBlock
flattenGroup = CodeBlock
             . intercalate [Line ""]
             . filter (not . null)
             . map runCodeBlock
             . runCodeGroup

flattenLayout :: CodeLayout -> CodeBlock
flattenLayout = CodeBlock
              . intercalate [Line "", Line ""]
              . filter (not . null)
              . map (runCodeBlock . flattenGroup)
              . runCodeLayout

-- |
-- separate by a single blank instead of a double blank, if reasonable.
-- 
-- >>> let wrapFoo block = CodeBlock [Line "foo {", Indented block, Line "}"]
-- >>> let simpleLayout = CodeLayout [singleLineGroup "bar", singleLineGroup "baz"]
-- >>> let complexLayout = CodeLayout [singleLineGroup "bar", CodeGroup [singleLineBlock "baz", singleLineBlock "quux"]]
-- 
-- This looks silly:
-- >>> testBlock $ wrapFoo $ flattenLayout simpleLayout
-- foo {
--   bar
--   .
--   .
--   baz
-- }
-- 
-- This is more reasonable:
-- >>> testBlock $ wrapFoo $ flattenLayout $ simplifyLayout simpleLayout
-- foo {
--   bar
--   .
--   baz
-- }
-- 
-- But it wouldn't be reasonable to simplify here:
-- >>> testBlock $ wrapFoo $ flattenLayout $ simplifyLayout complexLayout
-- foo {
--   bar
--   .
--   .
--   baz
--   .
--   quux
-- }
simplifyLayout :: CodeLayout -> CodeLayout
simplifyLayout codeLayout | hasOnlyDoubleBlanks codeLayout = useSingleBlanks codeLayout
                          | otherwise                      = codeLayout
  where
    hasSingleBlanks :: CodeGroup -> Bool
    hasSingleBlanks = (> 1) . length . runCodeGroup
    
    hasOnlyDoubleBlanks :: CodeLayout -> Bool
    hasOnlyDoubleBlanks = none hasSingleBlanks . runCodeLayout
    
    useSingleBlanks :: CodeLayout -> CodeLayout
    useSingleBlanks = multiBlockLayout
                    . foldMap runCodeGroup
                    . runCodeLayout


-- |
-- >>> printBlock $ flattenLayout codeLayout
-- This is a Line,
--   this is an
--   Indented block,
-- this is a second line,
--   this is a second indented block
--   consisting of four lines,
-- <BLANKLINE>
--   one of which is blank,
-- and this is a third line.
-- <BLANKLINE>
-- The above lines and indented blocks are grouped in a single CodeBlock.
-- These two lines are part of a second CodeBlock.
-- <BLANKLINE>
-- <BLANKLINE>
-- The last two blocks together are clumped in a CodeGroup.
-- This block
-- <BLANKLINE>
-- and this block
-- <BLANKLINE>
-- and this block
-- are all clumped into a second CodeGroup.
-- <BLANKLINE>
-- <BLANKLINE>
-- This third group completes the example.
-- Together, all three groups are clumped in a single CodeLayout.
printBlock :: CodeBlock -> IO ()
printBlock = go ""
  where
    go :: String -> CodeBlock -> IO ()
    go indent = mapM_ (printChunk indent) . runCodeBlock
    
    printChunk :: String -> CodeChunk -> IO ()
    printChunk indent (Line s) = putStr indent >> putStrLn s
    printChunk indent (Indented ss) = go ("  " ++ indent) ss

-- |
-- >>> testBlock $ flattenLayout codeLayout
-- This is a Line,
--   this is an
--   Indented block,
-- this is a second line,
--   this is a second indented block
--   consisting of four lines,
--   .
--   one of which is blank,
-- and this is a third line.
-- .
-- The above lines and indented blocks are grouped in a single CodeBlock.
-- These two lines are part of a second CodeBlock.
-- .
-- .
-- The last two blocks together are clumped in a CodeGroup.
-- This block
-- .
-- and this block
-- .
-- and this block
-- are all clumped into a second CodeGroup.
-- .
-- .
-- This third group completes the example.
-- Together, all three groups are clumped in a single CodeLayout.
testBlock :: CodeBlock -> IO ()
testBlock = printBlock . clarifyBlock
  where
    clarifyBlock :: CodeBlock -> CodeBlock
    clarifyBlock = CodeBlock . map clarifyChunk . runCodeBlock
    
    clarifyChunk :: CodeChunk -> CodeChunk
    clarifyChunk (Line "") = Line "."
    clarifyChunk (Line x) = Line x
    clarifyChunk (Indented xs) = Indented (clarifyBlock xs)
