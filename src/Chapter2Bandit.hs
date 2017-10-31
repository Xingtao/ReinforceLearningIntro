{-# LANGUAGE TemplateHaskell, RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Chapter2Bandit
    ( SampleAverageBandit(..)
    , doSampleAverage
    ) where

import Control.Lens (makeLenses, over, element, (+~), (&), (.~))
import Control.Monad.Random ( MonadRandom )
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
    -- if < 0, use N(A), then it is stationary;
    -- if > 0, use constant stepSize means non-stationary env, weight recent reward more.
    ,_stepSize :: Double
    ,_totalReward :: Double
    -- ramdoms
    ,_srcRVars :: [RVar Double]
    ,_greedyEpsilon :: RVar Bool
    }

makeLenses ''SampleAverageBandit

instance Show SampleAverageBandit where
  show saBandit = (show $ _kArms saBandit) ++ " arms, stepSize " ++ (show $ _stepSize saBandit)

takeOneAction :: Int -> SampleAverageBandit -> IO SampleAverageBandit
takeOneAction actionN saBandit = do  
  reward <- sample (_srcRVars saBandit !! actionN)
  let updateStepSize = _stepSize saBandit
      oldActionN = (_nActions saBandit) !! actionN
      oldValue = (_qValues saBandit) !! actionN
      newValue | updateStepSize < 0 = oldValue + (reward - oldValue) / (fromIntegral $ oldActionN + 1)
               | otherwise = oldValue + (reward - oldValue) / updateStepSize
  pure $ saBandit & (nActions . element actionN +~ 1)
                  & (qValues . element actionN .~ newValue)
                  & (totalReward +~ reward)

doSampleAverage :: Int -> SampleAverageBandit -> IO [Double]
doSampleAverage totalRun saBandit = go 1 saBandit
  where
  go count saBandit
    | count > totalRun = pure []
    | otherwise = do
        bExplore <- sample (_greedyEpsilon saBandit)
        actionN <- case bExplore of
                     True -> sample (randomElement [0..((_kArms saBandit) - 1) ])
                     False -> pure . fst $ argmaxWithIndex (zip [0..] (_qValues saBandit))
        newSA <- takeOneAction actionN saBandit
        ( ((_totalReward newSA) / (fromIntegral count)) :) <$> go (count + 1) newSA
