{-# LANGUAGE BangPatterns, DeriveFunctor, DeriveFoldable, DeriveTraversable, RecordWildCards #-}
module Data.AList
  ( AList
  , empty, singleton
  , fromMap, toMap
  , fromList, toList
  , mapWithKey
  , null, member, lookup, (!)
  , insert, union
  ) where

import Prelude hiding (null, lookup)

import qualified Data.Foldable as Foldable (toList)
import           Data.Hashable
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import           Data.Sequence (Seq, (|>), (><))
import qualified Data.Sequence as Seq


data AList k a = AList
  { keys :: !(Seq k)
  , values :: !(HashMap k a)
  } deriving (Show, Eq, Functor, Foldable, Traversable)

empty :: AList k a
empty = AList Seq.empty Map.empty

singleton :: Hashable k => k -> a -> AList k a
singleton k x = AList (Seq.singleton k) (Map.singleton k x)


fromMap :: HashMap k a -> AList k a
fromMap xs = AList (Seq.fromList ks) xs
  where
    ks = Map.keys xs

toMap :: AList k a -> HashMap k a
toMap = values

modifyMap :: (HashMap k a -> HashMap k b)
          -> AList k a -> AList k b
modifyMap f xs = xs { values = f (values xs) }


fromList :: (Eq k, Hashable k) => [(k, a)] -> AList k a
fromList xs = AList (Seq.fromList ks) (Map.fromList xs)
  where
    ks = map fst xs

toList :: (Eq k, Hashable k) => AList k a -> [(k, a)]
toList (AList {..}) = map go ks
  where
    ks = Foldable.toList keys
    go k = (k, values Map.! k)


mapWithKey :: (k -> a -> b)
           -> AList k a
           -> AList k b
mapWithKey = modifyMap . Map.mapWithKey


null :: AList k a -> Bool
null = Map.null . values

member :: (Eq k, Hashable k) => k -> AList k a -> Bool
member k = Map.member k . values

lookup :: (Eq k, Hashable k) => k -> AList k a -> Maybe a
lookup k = Map.lookup k . values

(!) :: (Eq k, Hashable k) => k -> AList k a -> a
(!) k = (Map.! k) . values


insert :: (Eq k, Hashable k) => k -> a -> AList k a -> AList k a
insert k x (AList {..}) = AList keys' values'
  where
    keys' | Map.member k values = keys
          | otherwise           = keys |> k
    values' = Map.insert k x values

union :: (Eq k, Hashable k) => AList k a -> AList k a -> AList k a
union xs1 xs2 = AList (keys xs1 >< ks2) (values xs1 `Map.union` values xs2)
  where
    notDuplicate = not . (`member` xs1)
    ks2 = Seq.filter notDuplicate (keys xs2)
