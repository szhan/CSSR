module Data.CSSR.Types where

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Vector as V

import CSSR.Prelude

-- | For the moment, an alphabet only consists of symbols of Chars
data Alphabet = Alphabet
  { idxToSym :: Vector Event
  , symToIdx :: HashMap Event Int
  } deriving (Eq)

mkAlphabet :: HashSet Event -> Alphabet
mkAlphabet alphas = Alphabet (V.fromList list) (HM.fromList $ zip list [0..])
  where
    list :: [Event]
    list = HS.toList alphas

instance Show Alphabet where
  -- a little convoluted in the case of strings
  show (Alphabet vec _) = "alphabet: [" ++ alphaList ++ "]"
    where
      alphaList :: String
      alphaList = intercalate "," (map show . V.toList $ vec)

-- data LoopingTree = LL
--   { obs :: String
--   , children :: HashMap Char LoopingTree
--   , isLoop :: Bool
--   , histories :: HashSet HistTree
--   }
--

