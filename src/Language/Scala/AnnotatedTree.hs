{-# LANGUAGE OverloadedStrings #-}
module Language.Scala.AnnotatedTree where

import Control.Arrow (second, (***))
import           Data.AList (AList)
import           Data.Yaml.Ordered (ToJSON(..))

import Data.Yaml.Ordered.MyExtra
import Language.Scala.Name


data ProductProps f = ProductProps
  { productFields :: AList ValName (TypeName, f)
  } deriving (Show, Eq)

data BranchProps f = BranchProps
  { branchFields :: AList ValName (TypeName, f)
  } deriving (Show, Eq)

data SumProps f b = SumProps
  { branches :: AList TypeName (BranchProps f, b)
  } deriving (Show, Eq)


data TopLevelType p s f b
  = TopLevelProduct (ProductProps f, p)
  | TopLevelSum (SumProps f b, s)
  deriving (Show, Eq)

-- p: product annotation
-- s: sum annotation
-- f: field annotation
-- b: branch annotation
newtype AnnotatedTree p s f b = AnnotatedTree
  { unAnnotatedTree :: AList TypeName (TopLevelType p s f b)
  } deriving (Show, Eq)


instance ToJSON f => ToJSON (ProductProps f) where
  toJSON (ProductProps x) = object [ "fields" .=! toJSON x ]

instance ToJSON f => ToJSON (BranchProps f) where
  toJSON (BranchProps x) = object [ "fields" .=! toJSON x ]

instance (ToJSON f, ToJSON b) => ToJSON (SumProps f b) where
  toJSON (SumProps x) = toJSON x

instance (ToJSON p, ToJSON s, ToJSON f, ToJSON b) => ToJSON (TopLevelType p s f b) where
  toJSON (TopLevelSum x) = toJSON x
  toJSON (TopLevelProduct x) = toJSON x

instance (ToJSON p, ToJSON s, ToJSON f, ToJSON b) => ToJSON (AnnotatedTree p s f b) where
  toJSON (AnnotatedTree x) = toJSON x


type Unannotated a = (a, ())

unannotated :: a -> Unannotated a
unannotated x = (x, ())

-- try to say "unUnannotated" 10 times fast :)
unUnannotated :: Unannotated a -> a
unUnannotated (x, ()) = x


mapProductProps :: (f1 -> f2)
                -> ProductProps f1
                -> ProductProps f2
mapProductProps ff (ProductProps xs) =
    ProductProps (fmap (second ff) xs)

mapBranchProps :: (f1 -> f2)
               -> BranchProps f1
               -> BranchProps f2
mapBranchProps ff (BranchProps xs) =
    BranchProps (fmap (second ff) xs)

mapSumProps :: (f1 -> f2)
            -> (b1 -> b2)
            -> SumProps f1 b1
            -> SumProps f2 b2
mapSumProps ff fb (SumProps xs) =
    SumProps (fmap (mapBranchProps ff *** fb) xs)

mapTopLevelTypes :: (p1 -> p2)
                 -> (s1 -> s2)
                 -> (f1 -> f2)
                 -> (b1 -> b2)
                 -> TopLevelType p1 s1 f1 b1
                 -> TopLevelType p2 s2 f2 b2
mapTopLevelTypes fp fs ff fb = go
  where
    go (TopLevelProduct xs) = TopLevelProduct ((mapProductProps ff    *** fp) xs)
    go (TopLevelSum     xs) = TopLevelSum     ((mapSumProps     ff fb *** fs) xs)

mapAnnotatedTree :: (p1 -> p2)
                 -> (s1 -> s2)
                 -> (f1 -> f2)
                 -> (b1 -> b2)
                 -> AnnotatedTree p1 s1 f1 b1
                 -> AnnotatedTree p2 s2 f2 b2
mapAnnotatedTree fp fs ff fb (AnnotatedTree xs) =
    AnnotatedTree (fmap (mapTopLevelTypes fp fs ff fb) xs)
