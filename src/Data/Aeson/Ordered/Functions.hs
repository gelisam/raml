-- |
-- Module:      Data.Aeson.Ordered.Functions
-- Copyright:   (c) 2011-2016 Bryan O'Sullivan
--              (c) 2011 MailRank, Inc.
-- License:     BSD3
-- Maintainer:  Bryan O'Sullivan <bos@serpentine.com>
-- Stability:   experimental
-- Portability: portable

module Data.Aeson.Ordered.Functions
    ( mapHashKeyVal
    , hashMapKey
    , mapKeyVal
    , mapKey
    ) where

import Control.Arrow (first, (***))
import Data.Hashable (Hashable)
import qualified Data.AList as H
import qualified Data.Map as M

-- | Transform a 'M.Map' into a 'H.AList' while transforming the keys.
mapHashKeyVal :: (Eq k2, Hashable k2) => (k1 -> k2) -> (v1 -> v2)
              -> M.Map k1 v1 -> H.AList k2 v2
mapHashKeyVal fk fv = H.fromList . map (fk *** fv) . M.toList
{-# INLINE mapHashKeyVal #-}

-- | Transform a 'M.Map' into a 'H.AList' while transforming the keys.
hashMapKey :: (Eq k1, Hashable k1, Ord k2) => (k1 -> k2)
           -> H.AList k1 v -> M.Map k2 v
hashMapKey fk = M.fromList . map (first fk) . H.toList
{-# INLINE hashMapKey #-}

-- | Transform the keys and values of a 'H.AList'.
mapKeyVal :: (Eq k2, Hashable k2) => (k1 -> k2) -> (v1 -> v2)
          -> H.AList k1 v1 -> H.AList k2 v2
mapKeyVal = H.mapKeyVal
{-# INLINE mapKeyVal #-}

-- | Transform the keys of a 'H.AList'.
mapKey :: (Eq k2, Hashable k2) => (k1 -> k2) -> H.AList k1 v -> H.AList k2 v
mapKey fk = mapKeyVal fk id
{-# INLINE mapKey #-}
