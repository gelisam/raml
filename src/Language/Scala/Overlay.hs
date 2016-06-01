module Language.Scala.Overlay where

import Data.IndentedCode
import Language.Scala.ScalaTree


data Overlay = Overlay
  { overlaidMethods :: CodeLayout
  , overlaidHelpers :: CodeLayout
  } deriving (Show, Eq)

type OverlaidTree = ScalaTree Overlay Overlay () ()


instance Monoid Overlay where
  mempty = Overlay mempty mempty
  Overlay xs1 ys1 `mappend` Overlay xs2 ys2 =
    Overlay (xs1 `mappend` xs2) (ys1 `mappend` ys2)
