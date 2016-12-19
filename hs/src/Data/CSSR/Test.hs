module Data.CSSR.Test where

import Control.Arrow ((&&&))
import Data.Vector (Vector)
import Data.Statistics.KologorovSmirnov


class Probabilistic a where
  frequency :: a -> Vector Integer

  totalCounts :: a -> Integer
  totalCounts a = foldr (+) 0 $ frequency a

  totalCounts_ :: a -> Double
  totalCounts_ = fromIntegral . totalCounts

  distribution :: a -> Vector Double
  distribution p = fmap ((/ total) . fromIntegral) . frequency $ p
    where
      total :: Double
      total = totalCounts_ p

test :: Probabilistic inst => inst -> inst -> Double -> Bool
test state testCase sig = nullHypothesis state testCase >= sig

nullHypothesis :: (Probabilistic empirical, Probabilistic test)
               => empirical -> test -> Double
nullHypothesis ss val = kstwo (countsAndDist ss) (countsAndDist val)
  where
    countsAndDist :: Probabilistic p => p -> (Integer, Vector Double)
    countsAndDist = totalCounts &&& distribution



