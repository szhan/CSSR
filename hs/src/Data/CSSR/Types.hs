module Data.CSSR.Types where

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Vector as V

import CSSR.Prelude

-- | For the moment, an alphabet only consists of symbols of Chars
data Alphabet = Alphabet
  { idxToSym :: Vector Event
  , symToIdx :: HashMap Event Int
  , set_ :: HashSet Event -- for testing
  } deriving (Show, Eq)

mkAlphabet :: HashSet Event -> Alphabet
mkAlphabet alphas = Alphabet (V.fromList list) (HM.fromList $ zip list [0..]) alphas
  where
    list :: [Event]
    list = HS.toList alphas

data HistTree = HL
  { ht_obs :: String
  , frequency :: Vector Integer
  , ht_children :: HashMap Char HistTree
  } deriving (Show)

data LoopingTree = LL
  { obs :: String
  , children :: HashMap Char LoopingTree
  , isLoop :: Bool
  , histories :: HashSet HistTree
  }


