{-# LANGUAGE TemplateHaskell #-}
module Data.Looping.Tree where

import Data.STRef
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import Data.Vector (Vector, (!))
import qualified Data.HashTable.ST.Cuckoo as C
import qualified Data.HashTable.Class as H
import Data.Foldable

import CSSR.Prelude
import Data.CSSR.Alphabet
import Data.Hist.Tree

-------------------------------------------------------------------------------
-- Looping Tree ADTs
-------------------------------------------------------------------------------
data LoopingTree = LoopingTree
  { _alphabet :: Alphabet
  , _root :: LLeaf
  } deriving (Eq)

data LLeaf = LLeaf
  { _body :: LLeafBody
  , _children :: HashMap Event LLeaf
  } deriving (Eq)

data LLeafBody = LLeafBody
  { _obs       :: Vector Event
  , _isLoop    :: Bool
  , _histories :: HashSet HLeaf
  , _frequency :: Vector Integer
  } deriving (Eq)

instance Show LoopingTree where
  show (LoopingTree a r) = "LoopingTree {"++ show a ++"}\n  root:" ++ show r

instance Show LLeaf where
  show = go 1 ' '
    where
      indent :: Int -> String
      indent d = replicate (5 * d) ' '

      showLeaf :: Int -> Event -> LLeafBody -> String
      showLeaf d e b = "\n" ++ indent d ++ show e ++"->LLeaf{" ++ show b

      go :: Int -> Event -> LLeaf -> String
      go d e (LLeaf b cs)
        | length cs == 0 = showLeaf d e b ++ ", no children}"
        | otherwise = showLeaf d e b ++ "}\n"
                      ++ indent (d + 1) ++ "children:"
                      ++ (intercalate "\n" . map (uncurry (go (d+1))) . HM.toList $ cs)

instance Show LLeafBody where
  show (LLeafBody o isL hs fq) = intercalate ", "
    [ "obs: " ++ show o
    , "isLoop: " ++ show isL
    , "histories: " ++ show hs
    , "freq: " ++ show fq
    ]

makeLenses ''LLeafBody
makeLenses ''LLeaf
makeLenses ''LoopingTree


