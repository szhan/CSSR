{-# LANGUAGE FlexibleInstances #-}
module CSSR.TypeAliases where

import Data.HashMap.Strict (HashMap)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Hashable

type Locations = HashMap Idx Integer
type Idx = Integer
type Event = String
type Delim = String
type DataFileContents = Vector Event

instance Hashable x => Hashable (Vector x) where
  hashWithSalt salt = hashWithSalt salt . V.toList


