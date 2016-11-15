{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module Main where

import GHC.Generics
import Criterion
import Criterion.Main
import qualified Data.Vector as V
import Data.Parse.Tree
import Control.DeepSeq
import Control.Exception

main :: IO ()
main = do
  let q = V.replicate 10000 '1'
  q' <- evaluate (force q)
  _ <- evaluate $ force $ buildTree 10 q'
  return ()

deriving instance Generic ParseTree
instance NFData ParseTree
deriving instance Generic PLeaf
instance NFData PLeaf
deriving instance Generic PLeafBody
instance NFData PLeafBody

