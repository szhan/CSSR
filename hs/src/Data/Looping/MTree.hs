{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Data.Looping.MTree where

import Data.STRef
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import Data.Vector (Vector, (!))
import Data.Vector.Mutable (MVector)
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Generic as GV
import qualified Data.HashTable.ST.Cuckoo as C
import qualified Data.HashTable.Class as H
import Data.Foldable

import CSSR.Prelude
import Data.CSSR.Alphabet
import Data.Hist.Tree (HLeaf, HLeafBody, HistTree(..))
import qualified Data.Hist.Tree as Hist

import Data.CSSR.Leaf.Probabilistic (Probabilistic)
import qualified Data.CSSR.Leaf.Probabilistic as Prob
import qualified Data.Looping.Tree as L

-------------------------------------------------------------------------------
-- Mutable Looping Tree ADTs
-------------------------------------------------------------------------------
data MLLeaf s = MLLeaf
  { _isLoop    :: STRef s Bool
  -- | for lack of a mutable hash set implementation
  , _histories :: C.HashTable s HLeaf Bool
  , _frequency :: MVector s Integer
  , _children :: C.HashTable s Event (MLLeaf s)
  , _parent :: STRef s (Maybe (MLLeaf s))
  }

instance Eq (MLLeaf s) where
  ml0 == ml1 = (_parent ml0 == _parent ml1)
    && (_isLoop ml0 == _isLoop ml1)

data MLoopingTree s = MLoopingTree
  { _terminals :: HashSet (MLLeaf s)
  , _root :: MLLeaf s
  }

freeze :: MLLeaf s -> ST s L.LLeaf
freeze ml = do
  il <- readSTRef . _isLoop $ ml
  hs <- freezeHistories ml
  f <- V.freeze . _frequency $ ml
  cs <- (H.toList . _children $ ml) >>= freezeDown
  return $ L.LLeaf (L.LLeafBody il hs f) cs Nothing

  where
    freezeHistories :: MLLeaf s -> ST s (HashSet Hist.HLeaf)
    freezeHistories = fmap (HS.fromList . fmap fst) . H.toList . _histories

    freezeDown :: [(Event, MLLeaf s)] -> ST s (HashMap Event L.LLeaf)
    freezeDown = fmap HM.fromList . traverse (\(e, cml) -> do
      c <- freeze cml
      return (e, c))

thaw :: L.LLeaf -> ST s (MLLeaf s)
thaw ll@(L.LLeaf lb cs p) = do
  il <- newSTRef (L.isLoop lb)
  hs <- H.fromList (fmap (,True) . HS.toList . L.histories $ lb)
  f <- V.thaw (L.frequency lb)
  cs <- thawDown (HM.toList . L.children $ ll)
  p <- newSTRef Nothing
  return $ MLLeaf il hs f cs p

  where
    thawDown :: [(Event, L.LLeaf)] -> ST s (C.HashTable s Event (MLLeaf s))
    thawDown cs = H.fromList =<< traverse (\(e, cml) -> do
      c <- thaw cml
      return (e, c)) cs


mkRoot :: Alphabet -> ST s (MLLeaf s)
mkRoot (Alphabet vec _) = do
  MLLeaf
    <$> newSTRef False
    <*> H.new
    <*> MV.replicate (V.length vec) 0
    <*> H.new
    <*> newSTRef Nothing

-- grow :: HistTree -> ST s (MLLeaf s)
-- grow (HistTree _ a hRoot) = do
--   rt <- mkRoot a
--   go [hRoot] rt
--   -- let findAlternative = LoopingTree.findAlt(ltree)(_)
--   return rt
--
--   where
--     go :: [HLeaf] -> MLLeaf s -> ST s ()
--     go             [] lf = return ()
--     go (active:queue) lf = go queue lf
--       where
--         isHomogeneous :: Bool
--         isHomogeneous = undefined

--   while (activeQueue.nonEmpty) {
--     val active:MLLeaf = activeQueue.remove(0)
--     val isHomogeneous:Boolean =
--                active
--                  .histories
--                  .forall{ LoopingTree.nextHomogeneous(tree) }
--
--     if (isHomogeneous) {
--       debug("we've hit our base case")
--     } else {
--
--       val nextChildren:Map[Char, LoopingTree.Node] = active.histories
--         .flatMap { _.children }
--         .groupBy{ _.observation }
--         .map { case (c, pleaves) => {
--           val lleaf:MLLeaf = new MLLeaf
--                              ( c + active.observed
--                              , pleaves
--                              , Option(active)
--                              )
--           val alternative
--                       :Option[LoopingTree.AltNode] = findAlternative(lleaf)
--           c -> alternative.toRight(lleaf)
--         } }
--
--       active.children ++= nextChildren
--       // Now that active has children, it cannot be considered a
--       // terminal node. Thus, we elide the active node:
--       ltree.terminals =
--           ltree.terminals ++
--           LoopingTree.leafChildren(nextChildren).toSet[MLLeaf] - active
--       // FIXME: how do edge-sets handle the removal of an active node?
--       // Also, are they considered terminal?
--       activeQueue ++= LoopingTree.leafChildren(nextChildren)
--     }
--   }
--
--   ltree
-- }



-------------------------------------------------------------------------------
-- Predicates for the consturction of a looping tree

-- | === isEdge
-- Psuedocode from paper:
--   INPUTS: looping node, looping tree
--   COLLECT all terminal nodes that are not ancestors
--   IF exists terminal nodes with identical distributions
--   THEN
--     mark looping node as an edge set
--     mark found terminals as an edge set
--     // We will merge edgesets in Phase III.
--   ENDIF

type EdgeGroup s = (Vector Integer, HashSet (MLLeaf s))

groupEdges :: forall s . Double -> MLoopingTree s -> ST s (HashSet (EdgeGroup s))
groupEdges sig (MLoopingTree terms _) = HS.foldr part (pure HS.empty) terms

  where
    --matchesDists_ :: Vector Integer -> Vector Integer -> Double -> Bool
    --matchesDists_ = kstwoTest_

    part :: MLLeaf s -> ST s (HashSet (EdgeGroup s)) -> ST s (HashSet (EdgeGroup s))
    part term groups' = do
      groups <- groups'
      found <- foundEdge
      case found of
        Nothing -> (\t -> HS.insert (t, HS.singleton term) groups) <$> termFreq
        Just g  -> updateGroup g groups

      where
        termFreq :: ST s (Vector Integer)
        termFreq = GV.basicUnsafeFreeze (_frequency term)

        updateGroup :: EdgeGroup s
                    -> HashSet (EdgeGroup s)
                    -> ST s (HashSet (EdgeGroup s))
        updateGroup g@(f, ts) groups = do
          summed <- summedST
          return $ HS.insert (summed, HS.insert term ts) (HS.delete g groups)


          where
            summedST :: ST s (Vector Integer)
            summedST = Prob.addFrequencies f <$> termFreq

        foundEdge :: ST s (Maybe (EdgeGroup s))
        foundEdge = do
          groups <- groups'
          foldrM matchEdges Nothing (HS.toList groups)

        matchEdges :: EdgeGroup s
                   -> Maybe (EdgeGroup s) -> ST s (Maybe (EdgeGroup s))
        matchEdges _  g@(Just _) = return g
        matchEdges g@(f, _) Nothing = do
          matched <- Prob.unsafeMatch (_frequency term) f sig
          return (if matched then Just g else Nothing)


-- | === Homogeneity
-- Psuedocode from paper:
--   INPUTS: looping node, parse tree
--   COLLECT all next-step histories from looping node in parse tree
--   FOR each history in next-step histories
--     FOR each child in history's children
--       IF child's distribution ~/=  node's distribution
--       THEN RETURN false
--       ENDIF
--     ENDFOR
--   ENDFOR
--   RETURN TRUE
--
isHomogeneous :: forall s . Double -> MLLeaf s -> ST s Bool
isHomogeneous sig ll = do
  pcs <- allPChilds
  foldrM step True pcs

  where
    allPChilds :: ST s (HashSet HLeaf)
    allPChilds = do
      let hs :: ST s [(HLeaf, Bool)]
          hs = H.toList (_histories ll)
      kvs <- hs
      let
        cs :: [HLeaf]
        cs = (fst <$> kvs) >>= HM.elems . view Hist.children
      return . HS.fromList $ cs

    step :: HLeaf -> Bool -> ST s Bool
    step _  False = return False
    step pc _     =
      Prob.unsafeMatch (_frequency ll) (Prob.frequency pc) sig

-- | === Excisability
-- Psuedocode from paper:
--   INPUTS: looping node, looping tree
--   COLLECT all ancestors of the looping node from the looping tree, ordered by
--           increasing depth (depth 0, or "root node," first)
--   FOR each ancestor
--     IF ancestor's distribution == looping node's distribution
--     THEN
--       the node is excisable: create loop in the tree
--       ENDFOR (ie "break")
--     ELSE do nothing
--     ENDIF
--   ENDFOR
--
excisable :: forall s . Double -> MLLeaf s -> ST s (Maybe (MLLeaf s))
excisable sig ll = getAncestors ll >>= go
  where
    go :: [MLLeaf s] -> ST s (Maybe (MLLeaf s))
    go [] = return Nothing
    go (a:as) = do
      isMatch <- Prob.unsafeMatch_ (_frequency ll) (_frequency a) sig
      if isMatch
      then return (Just a)
      else go as

-- | returns ancestors in order of how they should be processed
getAncestors :: MLLeaf s -> ST s [MLLeaf s]
getAncestors ll = go (Just ll) []
  where
    go :: Maybe (MLLeaf s) -> [MLLeaf s] -> ST s [MLLeaf s]
    go  Nothing ancestors = return ancestors
    go (Just w) ancestors = do
      p <- readSTRef (_parent w)
      go p (w:ancestors)



