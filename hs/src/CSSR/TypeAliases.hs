module CSSR.TypeAliases where

import Data.HashMap.Strict (HashMap)
import Data.Vector (Vector)

type Locations = HashMap Idx Integer
type Idx = Integer
type Event = Char
type DataFileContents = Vector Event



