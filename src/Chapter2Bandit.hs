{-# LANGUAGE TemplateHaskell, RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Chapter2Bandit
    ( Bandit(..)
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

import Numeric.LinearAlgebra (Vector, Matrix)
import qualified Numeric.LinearAlgebra as LA

import Utils

------------------------------------------------------------------------------------------
-- | Three methods: sample average, UCB, gradient bandit
--   For action selections (exploit & explore) of above methods:
--       1. sample average epsilon-greedy to do exploration.
--       2. UCB using:  Qt(a) + c sqrt(log t / Nt(a))
--       3. Gradient using Qt(a) as Preference value, select action by softmax
--   For Qt(a) (Value Estimate) Update
--       1. sample average using: newVal = oldVal + stepSize * (curReward - oldVal)
--       2. UCB using the same one with sample average
--       3. Gradient using gradient approximation:
--             newValue = oldValue + stepSize * (curReward - baselineVal) * ( 1 - softmax)
--             for the selected action
--         and newValue = oldValue + stepSize * (curReward - baselineVal) * softmax
--             for all other non-selected actions
--         where baseline could be 0, or the average value of all action rewards

data Bandit = Bandit {
     _kArms :: Int
    ,_qValues :: [Double]
    ,_bestValueIdx :: Int
    ,_nActions :: [Int]
    ,_greedyEpsilon :: Double
    -- if < 0, use N(A), then it is stationary;
    -- if > 0, use constant stepSize means non-stationary env, weight recent reward more.
    ,_stepSize :: Double
    ,_totalReward :: Double
    ,_curStep :: Int
    ,_totalStep :: Int
    ,_bestTakes :: [Double]
    -- input ramdoms
    ,_srcRVars :: [RVar Double]
    -- explore methods
    ,_greedyEpsilonRVar :: RVar Bool
    ,_ucbParam :: Double
    ,_gradientAlpha :: Double
    }

makeLenses ''Bandit

instance Show Bandit where
  show saBandit = (show $ _kArms saBandit) ++ " arms, stepSize " ++ (show $ _stepSize saBandit) ++
                  ", greedyEpsilon: " ++ (show $ _greedyEpsilon saBandit)

takeOneAction :: Bool -> Bandit -> IO Bandit
takeOneAction bExplore saBandit = do
  actionN <- case bExplore of
               True -> sample (randomElement [0..((_kArms saBandit) - 1)])
               False -> pure . fst . argmaxWithIndex $ (zip [0..] (_qValues saBandit))
  let bestTake = case (_bestValueIdx saBandit) == actionN of
                   True -> 1.0
                   False -> 0.0
  reward <- sample (_srcRVars saBandit !! actionN)
  let updateStepSize = _stepSize saBandit
      oldActionN = (_nActions saBandit) !! actionN
      oldValue = (_qValues saBandit) !! actionN
      newValue | updateStepSize < 0 = oldValue + (reward - oldValue)/(fromIntegral $ oldActionN + 1)
               | otherwise = oldValue + (reward - oldValue) / updateStepSize
      -- bestTakeList = _bestTakes saBandit
  pure $ (saBandit & (nActions . element actionN +~ 1)
                   & (qValues . element actionN .~ newValue)
                   & (totalReward +~ reward)
                   & (bestTakes %~ (++ [bestTake]))
         )

doSampleAverage :: Bandit -> IO (Vector Double)
doSampleAverage saBandit = do
  updatedRef <- newIORef saBandit
  averageRewards <- go 1 updatedRef saBandit
  saBandit' <- readIORef updatedRef
  -- print (_bestTakes saBandit')
  pure $ LA.fromList (averageRewards ++ (_bestTakes saBandit'))
  where
  go count updatedRef saBandit
    | count > (_totalStep saBandit) = writeIORef updatedRef saBandit >> pure []
    | otherwise = do
        bExplore <- sample (_greedyEpsilonRVar saBandit)
        newSA <- takeOneAction bExplore saBandit
        ( (_totalReward newSA) / (fromIntegral count) :) <$> go (count + 1) updatedRef newSA
