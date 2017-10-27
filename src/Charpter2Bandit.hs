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
    ,actionRewards :: [Double]
    }

-- mklenses ''Bandit

generateBandit :: Int -> [[Double]] -> IO Bandit
generateBandit k rewards =
  pure Bandit {
         greedyEpsilon = 0.1
        ,kArms = k
        ,qActions = take k (repeat 0.0)
        ,nActions = take k (repeat 0)
        ,actionRewards = []
       }

actionValues :: Int -> [Double] -> Double
actionValues k ar | k < (length ar) =  ar !! k
                  | otherwise = error ("kArms = " ++ show (length ar) ++ ", but index " ++ show k)
