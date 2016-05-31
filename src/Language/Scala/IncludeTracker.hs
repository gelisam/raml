module Language.Scala.IncludeTracker where

import Control.Monad.Trans.Writer.Strict
import Data.Monoid
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
--   let addValues :: [TypeName] -> CodeLayout
--       addValues = singleLineLayout
--                 . intercalate " + "
--                 . fmap (printf "%s.value")
-- :}
-- 
-- >>> let foo = qualifiedName "com.gelisam" "Foo"
-- >>> let bar = qualifiedName "com.gelisam" "Bar"
-- >>> let groupedCode = addValues <$> sequence [foo, bar, foo]
-- 
-- >>> testBlock $ flattenLayout $ runIncludeTracker Nothing groupedCode
-- import com.gelisam.Bar
-- import com.gelisam.Foo
-- .
-- .
-- Foo.value + Bar.value + Foo.value
-- 
-- >>> testBlock $ flattenLayout $ runIncludeTracker (Just "com.gelisam.adder") groupedCode
-- package com.gelisam.adder
-- .
-- import com.gelisam.Bar
-- import com.gelisam.Foo
-- .
-- .
-- Foo.value + Bar.value + Foo.value
runIncludeTracker :: Maybe PackageName
                  -> IncludeTracker CodeLayout
                  -> CodeLayout
runIncludeTracker packageName = uncurry go . runWriter
  where
    go :: CodeLayout -> Includes -> CodeLayout
    go codeLayout includes = multiBlockLayout [packageBlock, includeBlock] <> codeLayout
      where
        packageLine :: Maybe String
        packageLine = printf "package %s" <$> packageName
        
        includeLine :: IncludeName -> String
        includeLine = printf "import %s"
        
        packageBlock :: CodeBlock
        packageBlock = maybe mempty singleLineBlock packageLine
        
        includeBlock :: CodeBlock
        includeBlock = multiLineBlock
                     $ map includeLine
                     $ Set.toList includes
