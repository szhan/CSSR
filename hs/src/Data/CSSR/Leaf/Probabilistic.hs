module Data.CSSR.Leaf.Probabilistic where

import CSSR.Prelude
import qualified Data.Vector as V

class Probabilistic leaf where
  frequency :: leaf -> Vector Integer

  distribution :: leaf -> Vector Double
  distribution leaf = V.map (\f -> fromIntegral f / total) fs
    where
      total :: Double
      total = (fromIntegral . sum) fs

      fs :: Vector Integer
      fs = frequency leaf

  rounded :: leaf -> Vector Float
  rounded leaf = V.map (shorten 2) (distribution leaf)
    where
      shorten :: Int -> Double -> Float
      shorten n f = (fromInteger $ round $ f * (10^n)) / (10.0^^n)

matches :: (Probabilistic a, Probabilistic b) => a -> b -> Bool
matches a b = matches' (distribution a) (distribution b)
  where
    matches' :: Vector Double -> Vector Double -> Bool
    matches' = undefined



