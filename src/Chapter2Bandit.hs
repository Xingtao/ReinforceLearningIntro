{-# LANGUAGE TemplateHaskell, RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Chapter2Bandit
    ( SampleAverageBandit(..)
    , doSampleAverage
    ) where

import Control.Monad
import Control.Lens (makeLenses, over, element, (+~), (&), (.~))
import Data.List(take, repeat)
import Language.Haskell.TH

import Data.Random
import Data.Random.Distribution
import Data.Random.Distribution.Uniform
import Data.Random.Source.IO

import Utils

------------------------------------------------------------------------------------------
-- | Three methods: sample averate, gradient, ...

data SampleAverageBandit = SampleAverageBandit {
     _kArms :: Int
    ,_qValues :: [Double]
    ,_nActions :: [Int]
    ,_greedyEpsilon :: Double
    -- if < 0, use N(A), then it is stationary;
    -- if > 0, use constant stepSize means non-stationary env, weight recent reward more.
    ,_stepSize :: Double
    ,_totalReward :: Double
    ,_totalStep :: Int
    -- ramdoms
    ,_srcRVars :: [RVar Double]
    ,_greedyEpsilonRVar :: RVar Bool
    }

makeLenses ''SampleAverageBandit

instance Show SampleAverageBandit where
  show saBandit = (show $ _kArms saBandit) ++ " arms, stepSize " ++ (show $ _stepSize saBandit) ++
                  ", greedyEpsilon: " ++ (show $ _greedyEpsilon saBandit)

takeOneAction :: Bool -> SampleAverageBandit -> IO (Double, SampleAverageBandit)
takeOneAction bExplore saBandit = do
  let curBestActionIdx = fst $ argmaxWithIndex (zip [0..] (_qValues saBandit))
  actionN <- case bExplore of
               True -> sample (randomElement [0..((_kArms saBandit) - 1)])
               False -> pure curBestActionIdx
  let bestTake = case curBestActionIdx == actionN of
                   True -> 1.0
                   False -> 0.0
  reward <- sample (_srcRVars saBandit !! actionN)
  let updateStepSize = _stepSize saBandit
      oldActionN = (_nActions saBandit) !! actionN
      oldValue = (_qValues saBandit) !! actionN
      newValue | updateStepSize < 0 = oldValue + (reward - oldValue) / (fromIntegral $ oldActionN + 1)
               | otherwise = oldValue + (reward - oldValue) / updateStepSize
  pure $ (bestTake, saBandit & (nActions . element actionN +~ 1)
                             & (qValues . element actionN .~ newValue)
                             & (totalReward +~ reward)
         )

doSampleAverage :: SampleAverageBandit -> IO [(Double, Double)]
doSampleAverage saBandit = go 1 saBandit
  where
  go count saBandit
    | count > (_totalStep saBandit) = pure []
    | otherwise = do
        bExplore <- sample (_greedyEpsilonRVar saBandit)
        (bestTake, newSA) <- takeOneAction bExplore saBandit
        ( (bestTake, (_totalReward newSA) / (fromIntegral count)) :) <$> go (count + 1) newSA
