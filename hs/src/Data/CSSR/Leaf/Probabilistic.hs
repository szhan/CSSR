module Data.CSSR.Leaf.Probabilistic where

import CSSR.Prelude
import Data.Statistics.KologorovSmirnov
import qualified Data.Vector as V

class Probabilistic leaf where
  frequency :: leaf -> Vector Integer

  distribution :: leaf -> Vector Double
  distribution = freqToDist . frequency

  rounded :: leaf -> Vector Float
  rounded leaf = V.map (shorten 2) (distribution leaf)
    where
      shorten :: Int -> Double -> Float
      shorten n f = (fromInteger $ round $ f * (10^n)) / (10.0^^n)

matches :: (Probabilistic a, Probabilistic b) => a -> b -> Double -> Bool
matches a b = matchesDists (probabilisticToTuple a) (probabilisticToTuple b)


probabilisticToTuple :: Probabilistic a => a -> (Integer, Vector Double)
probabilisticToTuple a = (sum $ frequency a, distribution a)

matchesDists :: (Integer, Vector Double) -> (Integer, Vector Double) -> Double -> Bool
matchesDists = kstwoTest

matchesDists_ :: Vector Integer -> Vector Integer -> Double -> Bool
matchesDists_ = kstwoTest_

addFrequencies :: Vector Integer -> Vector Integer -> Vector Integer
addFrequencies = V.zipWith (+)

freqToDist :: Vector Integer -> Vector Double
freqToDist fs = V.map (\f -> fromIntegral f / total) fs
  where
    total :: Double
    total = (fromIntegral . sum) fs


