{-# OPTIONS_GHC -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      :  Control.Applicative.Infix
-- Copyright   :  (c) Anygma BVBA and Thomas Davie 2008
-- License     :  BSD3
-- 
-- Maintainer  :  tom.davie@gmail.com
-- Stability   :  experimental
-- 
-- Useful functions for bracketing infix operators -- providing
-- essentially infix liftA2.
-- @liftA2 (*) x y@ becomes @x <^(*)^> y@
----------------------------------------------------------------------

module Control.Applicative.Infix where

import Control.Applicative

infixl 3 <^
infixl 3 ^>
infixl 3 ↿
infixl 3 ↾

-- | Renaming of @flip fmap@.  Should be used in combination with @(^>)@ to
--   give infix @liftA2@:
--   @[1,2] <^(+)^> [2,3]@ == @[3,4,4,5]@
(<^),(↿) :: Functor f => f a -> (a -> b) -> f b
(<^) = flip (<$>)
(↿) = flip (<$>)

-- | Renaming of @(<*>)@.  Should be used in combination with @(<^)@ to give 
--   infix @liftA2@:
--   @[1,2] <^(+)^> [2,3]@ == @[3,4,4,5]@
(^>),(↾) :: Applicative f => f (a -> b) -> f a -> f b
(^>) = (<*>)
(↾) = (<*>)
