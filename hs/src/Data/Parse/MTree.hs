module Data.Parse.MTree where

import Control.Monad.ST
import Data.STRef
import Control.Exception (assert)
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import Data.Vector (Vector, (!))
import qualified Data.HashTable.ST.Cuckoo as C
import qualified Data.HashTable.Class as H
import Data.Foldable

import CSSR.Prelude
import CSSR.TypeAliases
import Data.Parse.Tree

data MPLeaf s = MPLeaf
  { obs_      :: Vector Event
  , count_    :: STRef s Integer
  , children_ :: C.HashTable s Event (MPLeaf s)
  }

mkMRoot :: ST s (MPLeaf s)
mkMRoot = do
  c <- newSTRef 0
  childs <- H.new
  return $ MPLeaf V.empty c childs

mkMPLeaf :: Vector Event -> ST s (MPLeaf s)
mkMPLeaf evts = do
  c <- newSTRef 1
  childs <- H.new
  return $ MPLeaf evts c childs

---------------------------------------------------------------------------------
-- We encounter the history say "110"
-- We go to the parse tree at the root
-- We take the 0 child of the root
-- We then take the 1 child of 0 (=10)
-- We then take the 1 child of 10 (=110)
--------------------------------------------------------------------------------
addPath :: Vector Event -> MPLeaf s -> ST s ()
addPath events = go (V.length events)
  where
    go :: Int -> MPLeaf s -> ST s ()
    go 0 _ = return ()
    go depth lf@(MPLeaf _ c childs) = do
      modifySTRef c (+1)
      mchild <- H.lookup childs h
      case mchild of
        Just child -> go (depth - 1) child
        Nothing -> go2 depth lf
      where
        h :: Event
        h = events ! (depth - 1)

    go2 :: Int -> MPLeaf s -> ST s ()
    go2 0 _ = return ()
    go2 depth (MPLeaf _ _ childs) = do
      lf <- mkMPLeaf $ V.drop (depth - 1) events
      H.insert childs h lf
      go2 (depth-1) lf
      where
        h :: Event
        h = events ! (depth - 1)


freeze :: MPLeaf s -> ST s PLeaf
freeze (MPLeaf o c childs) = do
  body <- PLeafBody o <$> readSTRef c <*> pure mempty
  childs' <- H.toList childs
  PLeaf body <$> (foldrM step mempty childs')
  where
    step :: (Event, MPLeaf s) -> HashMap Event PLeaf -> ST s (HashMap Event PLeaf)
    step (e, mlf) hm = HM.insert e <$> freeze mlf <*> pure hm



