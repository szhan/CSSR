module Data.Statistics.KologorovSmirnov where

import CSSR.Prelude
import Control.Exception
import Data.Function
import qualified Data.Vector as V

---------------------------------------------------------------------------------
-- | Kolmogorov-Smirnov Hypothesis Test:
--
-- Given an array @data1[1..n1]@, and an array @data2[1..n2]@, this routine
-- returns the whether or not their K-S statistic passes a hypothesis test
-- with a significance level of @a@. This tells us whether or not the data
-- sets are drawn from the same distribution as determined by the KS
-- distribution.
--
-- TODO: Look up tables for control values and verification
---------------------------------------------------------------------------------

-- | the number of observations and the sample's probability distribution
type CountsAndDist = (Integer, Vector Double)

kstwoTest :: CountsAndDist -> CountsAndDist
  -> Double -- ^the siginificance level of the test
  -> Bool -- ^whether or not the pvalue is greater than the significance
kstwoTest a b sig = kstwo a b > sig

kstwoTest_ :: Vector Integer -> Vector Integer
  -> Double -- ^the siginificance level of the test
  -> Bool -- ^whether or not the pvalue is greater than the significance
kstwoTest_ a b sig = kstwo (toTuple a) (toTuple b) > sig
  where
    toTuple :: Vector Integer -> CountsAndDist
    toTuple a = (sum a, freqToDist a)

    freqToDist :: Vector Integer -> Vector Double
    freqToDist fs = V.map (\f -> fromIntegral f / total) fs
      where
        total :: Double
        total = (fromIntegral . sum) fs




---------------------------------------------------------------------------------
-- | Kolmogorov-Smirnov probability (D > observed)
--
-- This routine returns the significance level for the hypothesis that the
-- data sets are drawn from the same distribution. Small values show that the
-- cumulative distribution function of @data1@ is significantly different
-- from that of @data2@.
--
-- This function is defined as:
--
-- Probability(D > observed) = Q_{ks}([\sqrt{N_e}+0.12+0.11+\sqrt{N_e}] D])
--
-- where D is the KS statistic and Q_{ks} is the calculation of significance,
-- defined  by:
--
-- Q_{ks}(\lambda) = 2 * \sum_{j=1,\infinity}(-1)^{j-1} e^{-2j^{2}\lamda^{2}}
--
-- and N_e is the effective number of datapoints, defined by:
--
-- N_e = \div{N_1 * N_2, N_1 + N_2}
--
-- Note from "Numerical Recipies in C":
-- > The nature of the approximation involved in [Probability(D > observed)]
-- > is that it becomes asymptotically accurate as the N_e becomes large, but
-- > is already quite good for Ne ≥ 4, as small a number as one might ever
-- > actually use.
--
---------------------------------------------------------------------------------

kstwo :: CountsAndDist -> CountsAndDist
  -> Double -- ^p-value, Probability(D > observed)
kstwo (n1', data1) (n2', data2) = probks $
  ksstatistic data1 data2 * (en + 0.12 + (0.11 / en))
  where
    -- the square root of two-sample effective size
    en, n1, n2 :: Double
    en = sqrt $ (n1 * n2) / (n1 + n2)

    n1 = fromIntegral n1'
    n2 = fromIntegral n2'

---------------------------------------------------------------------------------
-- the Kolmogorov-Smirnov statistic
---------------------------------------------------------------------------------
ksstatistic
  :: Vector Double -- ^sample one's pdf, used to calculate the first ecdf
  -> Vector Double -- ^sample two's pdf, used to calculate the second ecdf
  -> Double   -- ^the KS statistic: the supremum of the tow calculated ecdfs
ksstatistic data1 data2 =
  assert (length data1 == length data2) $
  foldr1 max $ fmap abs $ getAll data1 data2
  where
    -- | calc empirical cumulative distributions. Should have ascending order.
    ecdf :: Vector Double -> Vector Double
    ecdf = V.scanr (+) 0

    getAll :: Vector Double -> Vector Double -> Vector Double
    getAll d1 d2 = V.zipWith subtract (ecdf d1) (ecdf d2)

---------------------------------------------------------------------------------
-- | Kolmogorov-Smirnov probability function, Q_{ks}
--
-- Q_{ks}(\lambda) = 2 * \sum_{j=1,\infinity}(-1)^{j-1} e^{-2j^{2}\lamda^{2}}
--
-- This particular calculation assumes that we are working with discreteness
-- across the natural numbers.
---------------------------------------------------------------------------------
probks :: Double -> Double -- ^probability or 1.0 if @probks@ fails to converge.
probks alam {-alam stands for "a lambda", I believe-}= go (V.fromList [1..100]) 2 0 0
  where
    a2, eps1, eps2 :: Double
    a2 = -2 * (alam ** 2)
    eps1 = 0.001
    eps2 = 1.0e-8

    go :: Vector Double -> Double -> Double -> Double -> Double
    go js' fac oldsum termBF
      | V.null js' = 1         -- ^ Get here only by failing to converge.
      | (aterm <= eps1 * termBF) || (aterm  <= eps2 * newsum) = newsum
      | otherwise = go js (-1 * fac) newsum aterm
      where
        j = V.head js'
        js = V.tail js'

        term :: Double
        term = fac * exp (a2 * j * j)

        newsum :: Double
        newsum = oldsum + term

        aterm :: Double
        aterm = abs term

