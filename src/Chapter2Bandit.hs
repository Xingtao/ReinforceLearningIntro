{-# LANGUAGE TemplateHaskell, RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Chapter2Bandit
    ( SampleAverageBandit(..)
    , doSampleAverage
    ) where

import Control.Monad
import Control.Lens (makeLenses, over, element, (+~), (&), (.~), (%~))
import Data.List(take, repeat)
import Data.IORef

import Data.Random
import Data.Random.Distribution
import Data.Random.Distribution.Uniform
import Data.Random.Source.IO
import Language.Haskell.TH

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
    ,_bestTakes :: [Double]
    -- ramdoms
    ,_srcRVars :: [RVar Double]
    ,_greedyEpsilonRVar :: RVar Bool
    }

makeLenses ''SampleAverageBandit

instance Show SampleAverageBandit where
  show saBandit = (show $ _kArms saBandit) ++ " arms, stepSize " ++ (show $ _stepSize saBandit) ++
                  ", greedyEpsilon: " ++ (show $ _greedyEpsilon saBandit)

takeOneAction :: Bool -> SampleAverageBandit -> IO SampleAverageBandit
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
      -- bestTakeList = _bestTakes saBandit
  pure $ (saBandit & (nActions . element actionN +~ 1)
                   & (qValues . element actionN .~ newValue)
                   & (totalReward +~ reward)
                   & (bestTakes %~ ( ++ [bestTake]))
         )

doSampleAverage :: SampleAverageBandit -> IO (SampleAverageBandit, [Double])
doSampleAverage saBandit = do
  updatedRef <- newIORef saBandit
  averageRewards <- go 1 updatedRef saBandit
  saBandit' <- readIORef updatedRef
  pure (saBandit', averageRewards)
  where
  go count updatedRef saBandit
    | count > (_totalStep saBandit) = writeIORef updatedRef saBandit >> pure []
    | otherwise = do
        bExplore <- sample (_greedyEpsilonRVar saBandit)
        newSA <- takeOneAction bExplore saBandit
        ( (_totalReward newSA) / (fromIntegral count) :) <$> go (count + 1) updatedRef newSA
