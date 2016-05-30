module Language.Scala.IncludeTracker where

import Control.Monad.Trans.Writer.Strict
import           Data.Set (Set)
import qualified Data.Set as Set
import Text.Printf

import Data.IndentedCode
import Language.Scala.Name


type Includes = Set IncludeName
type IncludeTracker = Writer Includes


import_ :: IncludeName -> IncludeTracker ()
import_ = tell . Set.singleton

qualifiedName :: PackageName -> TypeName -> IncludeTracker TypeName
qualifiedName packageName typeName = do
    import_ $ printf "%s.%s" packageName typeName
    return typeName


-- |
-- >>> import Data.List
-- >>> :{
--   let addValues :: [TypeName] -> GroupedCode
--       addValues = return
--                 . return
--                 . unindentedLine
--                 . intercalate " + "
--                 . fmap (printf "%s.value")
-- :}
-- 
-- >>> let foo = qualifiedName "com.gelisam" "Foo"
-- >>> let bar = qualifiedName "com.gelisam" "Bar"
-- >>> let groupedCode = addValues <$> sequence [foo, bar, foo]
-- 
-- >>> printIndented $ layoutGroupedCode $ runIncludeTracker Nothing groupedCode
-- import com.gelisam.Bar
-- import com.gelisam.Foo
-- <BLANKLINE>
-- <BLANKLINE>
-- Foo.value + Bar.value + Foo.value
-- 
-- >>> printIndented $ layoutGroupedCode $ runIncludeTracker (Just "com.gelisam.adder") groupedCode
-- package com.gelisam.adder
-- <BLANKLINE>
-- import com.gelisam.Bar
-- import com.gelisam.Foo
-- <BLANKLINE>
-- <BLANKLINE>
-- Foo.value + Bar.value + Foo.value
runIncludeTracker :: Maybe PackageName
                  -> IncludeTracker GroupedCode
                  -> GroupedCode
runIncludeTracker packageName = uncurry go . runWriter
  where
    go :: GroupedCode -> Includes -> GroupedCode
    go code includes = [packageBlock, includeBlock] : code
      where
        packageLine :: Maybe String
        packageLine = printf "package %s" <$> packageName
        
        includeLine :: IncludeName -> String
        includeLine = printf "import %s"
        
        packageBlock :: IndentedCode
        packageBlock = maybe [] unindentedLine packageLine
        
        includeBlock :: IndentedCode
        includeBlock = unindentedLines
                     $ map includeLine
                     $ Set.toList includes
