{-# LANGUAGE ViewPatterns #-}
module Data.Parse.MTree where

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
    go dp lf@(MPLeaf _ c childs) = do
      modifySTRef c (+1)
      mchild <- H.lookup childs h
      case mchild of
        Just child -> go (dp - 1) child
        Nothing -> go2 dp lf
      where
        h :: Event
        h = events ! (dp - 1)

    go2 :: Int -> MPLeaf s -> ST s ()
    go2 0 _ = return ()
    go2 dp (MPLeaf _ _ childs) = do
      lf <- mkMPLeaf $ V.drop (dp - 1) events
      H.insert childs h lf
      go2 (dp-1) lf
      where
        h :: Event
        h = events ! (dp - 1)


freeze :: MPLeaf s -> ST s PLeaf
freeze (MPLeaf o c childs) = do
  bod <- PLeafBody o <$> readSTRef c <*> pure mempty
  childs' <- H.toList childs
  PLeaf bod <$> (foldrM step mempty childs')
  where
    step :: (Event, MPLeaf s) -> HashMap Event PLeaf -> ST s (HashMap Event PLeaf)
    step (e, mlf) hm = HM.insert e <$> freeze mlf <*> pure hm


buildMTree :: Int -> DataFileContents -> ST s (MPLeaf s)
buildMTree n' (V.filter isValid -> cs) = do
  rt <- mkMRoot
  forM_ [0 .. V.length cs] (\i -> addPath (sliceEvents i) rt)
  return rt
  where
    n :: Int
    n = n' + 1

    sliceEvents :: Int -> Vector Event
    sliceEvents i
      | i + n  <= length cs = V.slice i n cs -- ^get all children of depth
      | i + n' <= length cs = V.slice i (length cs - i) cs -- ^but also the depth
      | otherwise          = V.empty -- ^ignore all others

isValid :: Event -> Bool
isValid e = not . HS.member e . HS.fromList $ ['\r', '\n']

-------------------------------------------------------------------------------
-- Build a Parse Tree and get Alphabet
-------------------------------------------------------------------------------

buildTree :: Int -> DataFileContents -> ParseTree
buildTree n df = ParseTree n rt
  where
    rt = runST $ do
      rt' <- buildMTree n df
      lf <- freeze rt'
      return lf

getAlphabet :: ParseTree -> Alphabet
getAlphabet (ParseTree _ rt) = mkAlphabet $ go mempty [rt]
  where
    go :: HashSet Event -> [PLeaf] -> HashSet Event
    go es [] = es
    go es cs = go (HS.union es (keys pairs)) (map snd pairs)
      where
        pairs :: [(Event, PLeaf)]
        pairs = concatMap (HM.toList . view children) cs

        keys :: [(Event, PLeaf)] -> HashSet Event
        keys = HS.fromList . map fst


