{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Looping.Tree where

import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import Data.Vector (Vector, (!))
import Data.Vector.Mutable (MVector)
import qualified Data.Vector.Mutable as MV
import qualified Data.HashTable.ST.Cuckoo as C
import qualified Data.HashTable.Class as H
import Data.Foldable

import CSSR.Prelude
import Data.CSSR.Alphabet
import Data.Hist.Tree (HLeaf, HLeafBody, HistTree(..))
import qualified Data.Hist.Tree as Hist

import Data.CSSR.Leaf.Probabilistic (Probabilistic)
import qualified Data.CSSR.Leaf.Probabilistic as Prob

data LLeaf = LLeaf
  { body      :: LLeafBody
  , children  :: HashMap Event LLeaf
  , parent    :: Maybe LLeaf
  }

data LLeafBody = LLeafBody
  { isLoop    :: Maybe LLeaf
  , histories :: HashSet HLeaf
  , frequency :: Vector Integer
  } deriving (Show, Eq, Generic)

data LoopingTree = LoopingTree
  { _terminals :: HashSet LLeaf
  , _root :: LLeaf
  }

instance Eq LLeaf where
  (LLeaf b0 c0 _) == (LLeaf b1 c1 _) = b0 == b1 && c0 == c1

instance Show LLeaf where
  show (LLeaf b c _) = "LLeaf{"++ show b ++ ", " ++ show c ++"}"

instance Probabilistic LLeaf where
  frequency = Data.Looping.Tree.frequency . body

instance Hashable LLeafBody
instance Hashable LLeaf where
  hashWithSalt salt (LLeaf b _ _) = hashWithSalt salt b

--path :: forall f. Applicative f
--             => Vector Event
--             -> (LLeafBody -> f LLeafBody)
--             -> LLeaf
--             -> f LLeaf
--path events fn = go 0
--  where
--    go :: Int -> LLeaf -> f LLeaf
--    go dpth (LLeaf body childs _) =
--      if dpth == V.length events - 1
--      then LLeaf <$> fn body <*> pure childs <*> Nothing
--      else LLeaf <$> fn body <*> nextChilds <*> Nothing
--
--      where
--        nextChilds :: f (HashMap Event LLeaf)
--        nextChilds =
--          case HM.lookup c childs of
--            Just child -> HM.insert c <$> go (dpth + 1) child <*> pure childs
--            Nothing -> HM.insert c <$> buildNew dpth <*> pure childs
--          where
--            c :: Event
--            c = V.unsafeIndex events dpth
--
--
--        buildNew :: Int -> f LLeaf
--        buildNew d
--          | d == V.length events - 1 = LLeaf <$> mkBod events <*> pure mempty
--          | otherwise = LLeaf <$> mkBod es <*> childs_
--          where
--            c :: Event
--            c = V.unsafeIndex events (d + 1)
--
--            es :: Vector Event
--            es = V.take (d + 1) events
--
--            mkBod :: Vector Event -> f LLeafBody
--            mkBod es' = fn (LLeafBody es' 0 mempty)
--
--            childs_ :: f (HashMap Char LLeaf)
--            childs_ = HM.singleton c <$> buildNew (d + 1)



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
--
type EdgeGroup = (Vector Double, Vector Integer, HashSet LLeaf)

groupEdges :: Double -> LoopingTree -> HashSet EdgeGroup
groupEdges sig (LoopingTree terms _) = HS.foldr part HS.empty terms
  where
    part :: LLeaf -> HashSet EdgeGroup -> HashSet EdgeGroup
    part term groups =
      case foundEdge of
        Nothing -> HS.insert (termDist, termFreq, HS.singleton term) groups
        Just g  -> updateGroup g groups

      where
        termDist :: Vector Double
        termDist = Prob.distribution term

        termFreq :: Vector Integer
        termFreq = Prob.frequency term

        updateGroup :: EdgeGroup -> HashSet EdgeGroup -> HashSet EdgeGroup
        updateGroup g@(d, f, ts) groups =
          HS.insert (Prob.freqToDist summed, summed, HS.insert term ts) (HS.delete g groups)
          where
            summed :: Vector Integer
            summed = Prob.addFrequencies termFreq f

        foundEdge :: Maybe EdgeGroup
        foundEdge = HS.foldr matchEdges Nothing groups

        matchEdges :: EdgeGroup -> Maybe EdgeGroup -> Maybe EdgeGroup
        matchEdges _  g@(Just _) = g
        matchEdges g@(_, f, _) Nothing =
          if Prob.matchesDists_ (Prob.frequency term) f sig
          then Just g
          else Nothing


-- -- | === Homogeneity
-- -- Psuedocode from paper:
-- --   INPUTS: looping node, parse tree
-- --   COLLECT all next-step histories from looping node in parse tree
-- --   FOR each history in next-step histories
-- --     FOR each child in history's children
-- --       IF child's distribution ~/=  node's distribution
-- --       THEN RETURN false
-- --       ENDIF
-- --     ENDFOR
-- --   ENDFOR
-- --   RETURN TRUE
--
-- isHomogeneous :: Double -> LLeaf -> Bool
-- isHomogeneous sig ll = foldr step True allPChilds
--   where
--     allPChilds :: HashSet HLeaf
--     allPChilds = HS.fromList $
--       HS.toList (histories . body $ ll) >>= HM.elems . view Hist.children
--
--     step :: HLeaf -> Bool -> Bool
--     step _  False = False
--     step pc _     = Prob.matches ll pc sig
--
-- -- | === Excisability
-- -- Psuedocode from paper:
-- --   INPUTS: looping node, looping tree
-- --   COLLECT all ancestors of the looping node from the looping tree, ordered by
-- --           increasing depth (depth 0, or "root node," first)
-- --   FOR each ancestor
-- --     IF ancestor's distribution == looping node's distribution
-- --     THEN
-- --       the node is excisable: create loop in the tree
-- --       ENDFOR (ie "break")
-- --     ELSE do nothing
-- --     ENDIF
-- --   ENDFOR
-- --
-- excisable :: Double -> LLeaf -> Maybe LLeaf
-- excisable sig ll = go (getAncestors ll)
--   where
--     go :: [LLeaf] -> Maybe LLeaf
--     go [] = Nothing
--     go (a:as)
--       | Prob.matches ll a sig = Just a
--       | otherwise = go as
--
-- -- | returns ancestors in order of how they should be processed
-- getAncestors :: LLeaf -> [LLeaf]
-- getAncestors ll = go (Just ll) []
--   where
--     go :: Maybe LLeaf -> [LLeaf] -> [LLeaf]
--     go  Nothing ancestors = ancestors
--     go (Just w) ancestors = go (parent w) (w:ancestors)


