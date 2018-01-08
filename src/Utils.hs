{-# language ExtendedDefaultRules #-}
{-# language ScopedTypeVariables  #-}
{-# language OverloadedStrings #-}

module Utils where

import Control.Monad
import Data.List
import Data.Ord (comparing)

import Data.Random
import Data.Random.Distribution
import Data.Random.Source.IO
import Data.Random.Distribution.Bernoulli (bernoulli)

-- | generate a list of values via a provided random distribution
generateRandomList :: (Num a) => Int -> RVar a -> IO [a]
generateRandomList quatity rvar = go 0
  where 
  go count | count >= quatity = pure []
           | otherwise = sample rvar >>= \ x -> (x:) <$> go (count + 1)

-- | handy operator
(?) :: Bool -> (a, a) -> a
(?) bTrue (x, y) = if bTrue then x else y

factorial :: Integral a => a -> a
factorial n | n <= 0 = 1
            | otherwise = n * factorial (n-1)

-- | index with max value in a list
maxWithIndex :: (Ord a) => [(Int, a)] -> (Int, a)
maxWithIndex = maximumBy (comparing snd)

-- | Get max element by index of two list
maxElement :: (Ord a) => [a] -> [a] -> [a]
maxElement = zipWith (\ x y -> (x > y) ? (x, y))

minElement :: (Ord a) => [a] -> [a] -> [a]
minElement = zipWith (\ x y -> (x <= y) ? (x, y))

-- | argmax for a list of arguments
argmax :: (Ord b) => (a -> b) -> [a] -> a
argmax f []= error $ "'argmax' with empty list input"
argmax f (x:xs) = foldl' cmp x xs where cmp x y = (f x > f y) ? (x, y)

----------------------------------------------------------------------
-- epsilon greedy, also random select Hit or Stick
headOrTail :: Double -> IO Bool
headOrTail eps = sample $ bernoulli eps

randomFromRange :: (Enum a, Num a) => (a, a, a) -> IO a
randomFromRange (s, i, e) = sample (randomElement [s,(s+i)..e])

----------------------------------------------------------------------
-- tuple manipulate
mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (a, b) = (f a, f b)

addTuple :: Num a => (a, a) -> (a, a) -> (a, a)
addTuple (a1, b1) (a2, b2) = (a1 + a2, b1 + b2)

{-
import System.Random
-- 'Psuedo' Random Distributions
uniforms :: (Random a, Num a) => [a]
uniforms = randoms (mkStdGen 42)

uniforms' lo hi = randomRs (lo, hi) (mkStdGen 42)

-- | using Box-Muller function from 'normaldistribution' package

-- Box-Muller method generates two normally distributed independent
-- random values from two uniformly distributed independent random values.
boxMuller :: Floating a => a -> a -> (a, a)
boxMuller u1 u2 = 
  let r = sqrt (-2 * log u1)
      t = 2 * pi * u2
  in  (r * cos t, r * sin t)

-- List of uniformly distributed random values into normally distributed
-- Box-Muller method converts values two at a time, so discard last one if it is odd num
boxMullers :: Floating a => [a] -> [a]
boxMullers (u1:u2:us) = let (n1, n2) = boxMuller u1 u2 in n1 : n2 : boxMullers us 
boxMullers _ = []

-- 'Psuedo' normal distribution
normals = boxMullers $ randoms (mkStdGen 42)
-- uses the supplied (mean, standard deviation).
normals' (mean, sigma) g = map (\x -> x * sigma + mean) $ normals
-}
