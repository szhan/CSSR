{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
module Data.Looping.MTree where

import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Sequence as S
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Generic as GV
import qualified Data.HashTable.ST.Cuckoo as C
import qualified Data.HashTable.Class as H
import Data.Foldable

import CSSR.Prelude.Mutable
import Data.CSSR.Alphabet
import Data.Hist.Tree (HLeaf, HLeafBody, HistTree(..))
import qualified Data.Hist.Tree as Hist

import Data.CSSR.Leaf.Probabilistic (Probabilistic)
import qualified Data.CSSR.Leaf.Probabilistic as Prob
import qualified Data.Looping.Tree as L

-------------------------------------------------------------------------------
-- Mutable Looping Tree ADTs
-------------------------------------------------------------------------------
type HashTableSet s a = C.HashTable s a Bool

data MLLeaf s = MLLeaf
  -- | for lack of a mutable hash set implementation
  { histories :: HashTableSet s HLeaf
  , frequency :: MVector s Integer
  , children :: C.HashTable s Event (MLNode s)
  , parent :: STRef s (Maybe (MLLeaf s))
  }

type Loop s = MLLeaf s
type MLNode s = Either (Loop s) (MLLeaf s)

overlaps :: forall s . MLLeaf s -> MLLeaf s -> ST s Bool
overlaps l0 l1 = do
    p0 <- readSTRef $ parent l0
    p1 <- readSTRef $ parent l1
    return $ mvecEq l0 l1 && p0 == p1
    where
      mvecEq = MV.overlaps `on` frequency

instance Eq (MLLeaf s) where
  l0 == l1 = mvecEq l0 l1
    where
      mvecEq = MV.overlaps `on` frequency


data MLoopingTree s = MLoopingTree
  { _terminals :: HashSet (MLLeaf s)
  , _root :: MLLeaf s
  }

freeze :: forall s . Double -> MLLeaf s -> ST s L.LLeaf
freeze sig ml = do
  hs <- freezeHistories ml
  f  <- V.freeze . frequency $ ml
  cs' <- H.toList . children $ ml
  cs <- freezeDown cs'
  let cur = L.LLeaf (Right (L.LLeafBody hs f)) cs Nothing
  return $ withChilds cur (HM.map (withParent (Just cur)) cs)

  where
    withChilds :: L.LLeaf -> HashMap Event L.LLeaf -> L.LLeaf
    withChilds (L.LLeaf bod _ p) cs = L.LLeaf bod cs p

    withParent :: Maybe L.LLeaf -> L.LLeaf -> L.LLeaf
    withParent p (L.LLeaf bod cs _) = L.LLeaf bod cs p

    freezeHistories :: MLLeaf s -> ST s (HashSet Hist.HLeaf)
    freezeHistories = fmap (HS.fromList . fmap fst) . H.toList . histories

    freezeDown :: [(Event, MLNode s)] -> ST s (HashMap Event L.LLeaf)
    freezeDown cs = do
      frz <- traverse icer cs
      return $ HM.fromList frz
      where
        icer :: (Event, MLNode s) -> ST s (Event, L.LLeaf)
        icer (e, Left lp) = do
          f <- V.freeze $ frequency lp
          hs <- (fmap.fmap) fst $ H.toList (histories lp)
          c <- freeze sig lp
          return (e, c)
        icer (e, Right lp) = do
          c <- freeze sig lp
          hs <- (fmap.fmap) fst $ H.toList (histories lp)
          return (e, c)


mkLeaf :: Maybe (MLLeaf s) -> [HLeaf] -> ST s (MLLeaf s)
mkLeaf p' hs' = do
  il <- newSTRef Nothing
  hs <- H.fromList $ fmap (, True) hs'
  let v = foldr1 Prob.addFrequencies $ fmap (view (Hist.body . Hist.frequency)) hs'
  mv <- GV.thaw v
  cs <- H.new
  p <- newSTRef p'
  return $ MLLeaf hs mv cs p

mkRoot :: Alphabet -> HLeaf -> ST s (MLLeaf s)
mkRoot (Alphabet vec _) hrt =
  MLLeaf
    <$> H.fromList [(hrt, True)]
    <*> MV.replicate (V.length vec) 0
    <*> H.new
    <*> newSTRef Nothing

walk :: forall s . MLNode s -> Vector Event -> ST s (Maybe (MLNode s))
walk cur es
  | null es = return $ Just cur
  | otherwise = do
    f <- H.lookup (children (reify cur)) (V.head es)
    case f of
      Nothing -> return Nothing
      Just nxt -> walk nxt (V.tail es)
  where
    reify :: MLNode s -> MLLeaf s
    reify (Left  l) = l
    reify (Right l) = l

-------------------------------------------------------------------------------
-- | == Phase II: "Growing a Looping Tree" algorithm
--
-- INIT root looping node
-- INIT queue of active, unchecked nodes
-- QUEUE root
-- WHILE queue is not empty
--   DEQUEUE first looping node from the queue
--   COMPUTE homogeneity(dequeued looping node, parse tree)
--   IF node is homogeneous
--   THEN continue
--   ELSE
--     CONSTRUCT new looping nodes for all valid children (one for each symbol in
--               alphabet - must have empirical observation in dataset).
--     FOR each new node constructed
--       COMPUTE excisability(node, looping tree)
--       ADD all new looping nodes to children of active node (mapped by symbol)
--       ADD unexcisable children to queue (FIXME: what about edgesets?)
--   ENDIF
-- ENDWHILE
-------------------------------------------------------------------------------
grow :: forall s . Double -> HistTree -> ST s (MLLeaf s)
grow sig (HistTree _ a hRoot) = do
  rt <- mkRoot a hRoot   -- ^ INIT root looping node
  ts <- newSTRef [rt]    -- ^ INIT queue of active, unchecked nodes
                         --   QUEUE root
  go (S.singleton rt) ts
  return rt
  where
    go :: Seq (MLLeaf s) -> STRef s [MLLeaf s] -> ST s ()
    go queue termsRef             -- ^ DEQUEUE first looping node from the queue
      | S.null queue = return ()
      | otherwise = do            -- ^ WHILE queue is not empty
        terms <- readSTRef termsRef
        isH <- isHomogeneous sig active
        if isH
        then go next termsRef
        else do
          cs' <- nextChilds
          let cs'' = fmap snd cs'
          -- COMPUTE excisability(node, looping tree)
          cs <- mapM (\(e, x) -> do
            x' <- findLoops x
            return (e, x')) cs'

          forM_ cs (\(e, x) -> H.insert (children active) e x)
          writeSTRef termsRef (cs'' <> delete active terms)
          --   ADD all new looping nodes to children of active (mapped by symbol)
          --   ADD unexcisable children to queue (FIXME: what about edgesets?)
          go (next <> S.fromList cs'') termsRef

      where
        findLoops :: MLLeaf s -> ST s (MLNode s)
        findLoops ll =
          excisable sig ll >>= \case
            Nothing -> return $ Right ll
            Just ex -> return $ Left ex

        next :: Seq (MLLeaf s)
        (active', next) = S.splitAt 1 queue

        active :: MLLeaf s
        active = S.index active' 0

        -- CONSTRUCT new looping nodes for all valid children
        --    (one for each symbol in alphabet - must have empirical
        --    observation in dataset).
        nextChilds :: ST s [(Event, MLLeaf s)]
        nextChilds = do
          hs <- (fmap.fmap) fst . H.toList . histories $ active
          traverse (\(e, _hs) -> (e,) <$> mkLeaf (Just active) _hs) $ groupHistory hs

        groupHistory :: [HLeaf] -> [(Event, [HLeaf])]
        groupHistory = groupBy (V.head . view (Hist.body . Hist.obs))

-------------------------------------------------------------------------------
-- Predicates for the construction of a looping tree

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
--
--type EdgeGroup s = (Vector Integer, HashSet (MLLeaf s))
--
--groupEdges :: forall s . Double -> MLoopingTree s -> ST s (HashSet (EdgeGroup s))
--groupEdges sig (MLoopingTree terms _) = HS.foldr part (pure HS.empty) terms
--
--  where
--    --matchesDists_ :: Vector Integer -> Vector Integer -> Double -> Bool
--    --matchesDists_ = kstwoTest_
--
--    part :: MLLeaf s -> ST s (HashSet (EdgeGroup s)) -> ST s (HashSet (EdgeGroup s))
--    part term groups' = do
--      groups <- groups'
--      found <- foundEdge
--      case found of
--        Nothing -> (\t -> HS.insert (t, HS.singleton term) groups) <$> termFreq
--        Just g  -> updateGroup g groups
--
--      where
--        termFreq :: ST s (Vector Integer)
--        termFreq = GV.basicUnsafeFreeze (frequency term)
--
--        updateGroup :: EdgeGroup s
--                    -> HashSet (EdgeGroup s)
--                    -> ST s (HashSet (EdgeGroup s))
--        updateGroup g@(f, ts) groups = do
--          summed <- summedST
--          return $ HS.insert (summed, HS.insert term ts) (HS.delete g groups)
--
--
--          where
--            summedST :: ST s (Vector Integer)
--            summedST = Prob.addFrequencies f <$> termFreq
--
--        foundEdge :: ST s (Maybe (EdgeGroup s))
--        foundEdge = do
--          groups <- groups'
--          foldrM matchEdges Nothing (HS.toList groups)
--
--        matchEdges :: EdgeGroup s
--                   -> Maybe (EdgeGroup s) -> ST s (Maybe (EdgeGroup s))
--        matchEdges _  g@(Just _) = return g
--        matchEdges g@(f, _) Nothing = do
--          matched <- Prob.unsafeMatch (frequency term) f sig
--          return (if matched then Just g else Nothing)


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
          hs = H.toList (histories ll)
      kvs <- hs
      let
        cs :: [HLeaf]
        cs = (fst <$> kvs) >>= HM.elems . view Hist.children
      return . HS.fromList $ cs

    step :: HLeaf -> Bool -> ST s Bool
    step _  False = return False
    step pc _     =
      Prob.unsafeMatch (frequency ll) (Prob.frequency pc) sig

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
      isMatch <- Prob.unsafeMatch_ (frequency ll) (frequency a) sig
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
      p <- readSTRef (parent w)
      go p (w:ancestors)



