{-# LANGUAGE BangPatterns, DeriveAnyClass, DeriveDataTypeable, DeriveGeneric, DeriveFunctor, DeriveFoldable, DeriveTraversable, GeneralizedNewtypeDeriving, RecordWildCards #-}
module Data.AList
  ( AList
  , empty, singleton
  , fromMap, toMap
  , fromList, toList
  , mapKeyVal, mapWithKey, foldrWithKey, traverseWithKey
  , null, member, lookup, (!)
  , insert, union, unionWith
  ) where

import Prelude hiding (null, lookup)

import Control.Arrow ((***))
import Control.DeepSeq (NFData(..))
import GHC.Generics (Generic)
import           Data.Data
import qualified Data.Foldable as Foldable
import           Data.Hashable
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import           Data.Sequence (Seq, (|>), (><))
import qualified Data.Sequence as Seq


data AList k a = AList
  { keys :: !(Seq k)
  , values :: !(HashMap k a)
  } deriving (Data, Eq, Foldable, Functor, Generic, NFData, Read, Show, Traversable)

instance (Hashable k, Hashable a) => Hashable (AList k a) where
  hashWithSalt salt (AList {..}) = salt
                    `hashWithSalt` Foldable.toList keys
                    `hashWithSalt` values


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


mapKeyVal :: (Eq k2, Hashable k2)
          => (k1 -> k2)
          -> (a1 -> a2)
          -> AList k1 a1 -> AList k2 a2
mapKeyVal f g (AList {..}) = AList (fmap f keys) (mapG values)
  where
    mapG = Map.fromList . map (f *** g) . Map.toList

mapWithKey :: (k -> a -> b)
           -> AList k a
           -> AList k b
mapWithKey = modifyMap . Map.mapWithKey

foldrWithKey :: (Eq k, Hashable k)
             => (k -> a -> s -> s)
             -> s -> AList k a -> s
foldrWithKey f s0 (AList {..}) = Foldable.foldr (\k -> f k (values Map.! k)) s0 keys

traverseWithKey :: Applicative f
                => (k -> a -> f b)
                -> AList k a -> f (AList k b)
traverseWithKey f (AList {..}) = AList keys <$> Map.traverseWithKey f values


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

union :: (Eq k, Hashable k)
      => AList k a -> AList k a -> AList k a
union = unionWith const

unionWith :: (Eq k, Hashable k)
          => (a -> a -> a)
          -> AList k a -> AList k a -> AList k a
unionWith f xs1 xs2 = AList (keys xs1 >< ks2)
                            (Map.unionWith f (values xs1) (values xs2))
  where
    notDuplicate = not . (`member` xs1)
    ks2 = Seq.filter notDuplicate (keys xs2)
