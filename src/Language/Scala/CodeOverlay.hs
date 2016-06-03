module Language.Scala.CodeOverlay where

import Data.IndentedCode


data CodeOverlay = CodeOverlay
  { overlaidMethods :: CodeLayout
  , overlaidHelpers :: CodeLayout
  } deriving (Show, Eq)


instance Monoid CodeOverlay where
  mempty = CodeOverlay mempty mempty
  CodeOverlay xs1 ys1 `mappend` CodeOverlay xs2 ys2 =
    CodeOverlay (xs1 `mappend` xs2) (ys1 `mappend` ys2)


singleMethodOverlay :: CodeBlock -> CodeOverlay
singleMethodOverlay x = CodeOverlay (singleBlockLayout x) mempty

multiMethodOverlay :: [CodeBlock] -> CodeOverlay
multiMethodOverlay x = CodeOverlay (multiBlockLayout x) mempty


singleHelperOverlay :: CodeBlock -> CodeOverlay
singleHelperOverlay x = CodeOverlay mempty (singleBlockLayout x)

multiHelperOverlay :: [CodeBlock] -> CodeOverlay
multiHelperOverlay x = CodeOverlay mempty (multiBlockLayout x)
