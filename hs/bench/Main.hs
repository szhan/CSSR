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
main = defaultMain $ fmap tester [10000]-- [500,1000..10000]
  where
    test :: Int -> V.Vector Char -> Benchmark
    test count vec = bench ("buildTree " ++ show count) $ nf (buildTree 10) vec

    tester count = env (pure $ V.replicate count '1') (test count)


deriving instance Generic ParseTree
instance NFData ParseTree
deriving instance Generic PLeaf
instance NFData PLeaf
deriving instance Generic PLeafBody
instance NFData PLeafBody

