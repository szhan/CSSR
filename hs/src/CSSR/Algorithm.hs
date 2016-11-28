-------------------------------------------------------------------------------
-- |
-- The CSSR Algorithm. This module exemplifies how the components of CSSR
-- connect together into a cohesive algorithm
-------------------------------------------------------------------------------
module CSSR.Algorithm where

import qualified Data.Vector as V
import qualified Data.Parse.MTree as M
import Data.Hist.Tree


-------------------------------------------------------------------------------
-- |
-- == Phase I: "Initialization"
--
-- Requires estimates of conditional probabilities to converge, perhaps rapidly.
-------------------------------------------------------------------------------
main :: FilePath -> IO ()
main filepath = do
  contents <- readFile filepath
  let histTree = initialization contents
  return ()

initialization :: String -> HistTree
initialization = uncurry convert . M.buildTree 4 . V.fromList


