module Data.CSSR.Test where

import Data.Vector (Vector)
import Data.Function (on)
import Data.Statistics.KologorovSmirnov


class Probabilistic a where
  frequency :: Vector Integer

  totalCounts :: Integer
  totalCounts = foldr (+) frequency

  distribution :: Vector Double
  distribution = map (// totalCounts) frequency
    where
      (//) :: Integer -> Integer -> Double
      a // b = (/) `on` fromIntegral

test :: Probabilistic inst => inst -> inst -> Double -> Bool
test state testCase sig = nullHypothesis state testCase >= sig


nullHypothesis :: (Probabilistic empirical, Probabilistic test)
               => empirical -> test -> Double
nullHypothesis ss val = kstwo state_dist state_total test_dist testCase_total
  where
    state_dist = distribution state
    state_total = totalCounts state
    test_dist = distribution testCase
    test_total = totalCounts testCase



