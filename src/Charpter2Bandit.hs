module Charpter2Bandit
    ( Bandit(..)
    , generateBandit
    ) where

import Control.Lens
import Data.List(take, repeat)
import Data.Random.Normal

data Bandit = Bandit {
     greedyEpsilon :: Double
    ,kArms :: Int
    ,qActions :: [Double]
    ,nActions :: [Int]
    ,actionRewards :: [[Double]]
    }

mklenses ''Bandit

generateBandit :: Int -> [[Dboule]] -> IO Bandit
generateBandit k rewards =
  pure Bandit {
   kArms = k
  ,qActions = take kArms (repeat 0.0)
  ,nActions = take kArms (repeat 0)
  ,actionRewards
  }

actionValues :: Int -> Int -> IO Double
actionValues k n avs | k < kArms =  avs !! k !! n
                     | otherwise = error ("kArms = " ++ show kArms ++ ", but index " ++ show k)
