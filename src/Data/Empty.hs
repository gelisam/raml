-- Normally I'd use Monoid, but I need a type class whose instance for Maybe
-- doesn't require an instance on the index.
module Data.Empty where

import Prelude hiding (null)

import           Data.AList (AList)
import qualified Data.AList as AList
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set


class Empty a where
  null :: a -> Bool

instance Empty (Maybe a) where
  null Nothing = True
  null _ = False

instance Empty [a] where
  null [] = True
  null _ = False

instance Empty (Map k a) where
  null = Map.null

instance Empty (AList k a) where
  null = AList.null

instance Empty (Set a) where
  null = Set.null
