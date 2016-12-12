module Data.CSSR.Leaf.Probabilistic where

import CSSR.Prelude
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

matches :: (Probabilistic a, Probabilistic b) => a -> b -> Bool
matches a b = matchesDists (distribution a) (distribution b)

matchesDist :: Probabilistic a => a -> Vector Double -> Bool
matchesDist a = matchesDists (distribution a)

matchesDists :: Vector Double -> Vector Double -> Bool
matchesDists = undefined

addFrequencies :: Vector Integer -> Vector Integer -> Vector Integer
addFrequencies = V.zipWith (+)

freqToDist :: Vector Integer -> Vector Double
freqToDist fs = V.map (\f -> fromIntegral f / total) fs
  where
    total :: Double
    total = (fromIntegral . sum) fs


