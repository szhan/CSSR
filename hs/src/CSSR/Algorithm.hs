-------------------------------------------------------------------------------
-- |
-- The CSSR Algorithm. This module exemplifies how the components of CSSR
-- connect together into a cohesive algorithm
-------------------------------------------------------------------------------
{-# LANGUAGE ViewPatterns #-}

module CSSR.Algorithm where

import qualified Data.Vector as V
import qualified Data.Parse.MTree as M
import Data.Parse.Tree
import Data.Hist.Tree
import qualified Data.Parse.MTree as MHist
import Debug.Trace


-------------------------------------------------------------------------------
-- |
-- == Phase I: "Initialization"
--
-- Requires estimates of conditional probabilities to converge, perhaps rapidly.
-------------------------------------------------------------------------------
main :: FilePath -> IO ()
main filepath = do
  contents <- readFile filepath
  let histTree = charInitialization 1 contents
  print histTree
  return ()

charInitialization :: Int -> String -> HistTree
charInitialization depth (fmap (:[]) -> s) = (convert parseTree $ M.getAlphabet parseTree)
  where
    parseTree :: ParseTree
    parseTree = M.buildTree depth . V.fromList $ s

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
--       COMPUTE isEdge(node, looping tree)
--       ADD all new looping nodes to children of active node (mapped by symbol)
--       ADD unexcisable children to queue (FIXME: what about edgesets?)
--   ENDIF
-- ENDWHILE
--
-- isEdge:
--   INPUTS: looping node, looping tree
--   COLLECT all terminal nodes that are not ancestors
--   IF exists terminal nodes with identical distributions
--   THEN
--     mark looping node as an edge set
--     mark found terminals as an edge set
--     // We will merge edgesets in Phase III.
--   ENDIF
--
-------------------------------------------------------------------------------
