{-# LANGUAGE TemplateHaskell, RankNTypes #-}

module Chapter2Bandit
    ( SampleAverageBandit(..)
    , generateBandit
    ) where

import Control.Lens
import Control.Monad.Random ( MonadRandom )
import Data.List(take, repeat)
import Language.Haskell.TH

data SampleAverageBandit = SampleAverageBandit {
     _kArms :: Int
    ,_greedyEpsilon :: Double
    ,_qValues :: [Double]
    ,_nActions :: [Int]
    ,_stepSize :: Int -> Double
    }

makeLenses ''SampleAverageBandit

generateBandit :: Int -> Double -> [Double] -> Double -> SampleAverageBandit
generateBandit k ge initRewards stepValue =
  SampleAverageBandit k ge initRewards (take k (repeat 0)) stepValue
  
actionValues :: Int -> [Double] -> Double
actionValues k ar | k < (length ar) =  ar !! k
                  | otherwise = error ("kArms = " ++ show (length ar) ++ ", but index " ++ show k)
