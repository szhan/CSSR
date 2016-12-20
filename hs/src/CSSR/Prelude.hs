{-# LANGUAGE ScopedTypeVariables #-}
module CSSR.Prelude
  ( X.Vector
  , X.HashMap
  , X.HashSet
  , Generic

  , groupBy

  , module Lens.Micro.Platform
  , module Data.Monoid
  , module Control.Monad.ST
  , module Debug.Trace
  , module Data.List
  , module Data.Maybe
  , module Control.Exception
  , module Data.Hashable

  , module CSSR.TypeAliases
  ) where

import qualified Data.Vector as X (Vector)
import qualified Data.HashMap.Strict as X (HashMap)
import qualified Data.HashSet as X (HashSet)
import qualified Data.HashMap.Strict as HM

import Control.Monad.ST
import Lens.Micro.Platform
import Data.Monoid
import Data.List hiding (groupBy)
import Data.Maybe
import CSSR.TypeAliases
import Debug.Trace
import Control.Exception
import GHC.Generics (Generic)
import Data.Hashable

groupBy :: forall a b . (Hashable b, Eq b) => (a -> b) -> [a] -> [(b, [a])]
groupBy fn = HM.toList . foldr step mempty
  where
    step :: a -> X.HashMap b [a] -> X.HashMap b [a]
    step a memo = HM.alter go (fn a) memo
      where
        go :: Maybe [a] -> Maybe [a]
        go   Nothing = Just [a]
        go (Just as) = Just (a:as)

