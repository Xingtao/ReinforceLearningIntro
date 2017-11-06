{-# LANGUAGE TemplateHaskell, RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE MultiParamTypeClasses       #-}
{-# LANGUAGE TypeOperators               #-}

module Chapter2Bandit
    ( Bandit(..)
    , doSampleAverage
    ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State
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

-- | Bandit Problem
--   Three methods (Policies): epcilong-greedy, UCB, gradient bandit.
--   Policy includes two parts: action selection & state-value estimation update.

-- | For action selections (exploit & explore) of above methods:
--   1. epsilon-greedy: randomly do exploration with probability epsilon;
--   2. UCB using: Qt(a) + c sqrt(log t / Nt(a)) to balance exploit and explore
--   3. Gradient using Qt(a) as Preference value, select action by softmax;

-- | For Qt(a) (Value Estimate) Update:
--   1. 'epsilon-greedy' & 'UCB' methods using form of:
--         newVal = oldVal + stepSize * (curReward - oldVal)
--      where 'stepSize' could use 'sample average' or 'exponential, ecency-weighted average'(constant stepSize).
--      here, 'stepSize' parameter in configuration file:
--        if < 0, use 'sample average', then it is stationary;
--        if > 0, use 'exponential, ecency-weighted average', then it's non-stationary, weight recent reward more.
--   2. Gradient bandit using gradient approximation:
--           newValue = oldValue + stepSize * (curReward - baselineVal) * ( 1 - softmax)
--           for the selected action's state value update
--       and newValue = oldValue + stepSize * (curReward - baselineVal) * softmax
--           for all other non-selected actions'
--       where 'baseline' could be 0, or the average value of all action rewards until the time
--       and 'stepSize' is the same as above.

------------------------------------------------------------------------------------------

data Policy = EGreedy (RVar Bool) -- Bernoulli distribution with p = epsilon
            | UCB Double -- ucb explore parameter 'c'
            | Gradient Double -- gradient baseline param, if < 0, use average reward value

data Bandit = Bandit {
     _kArms :: Int
    ,_bestValueIdx :: Int -- it is pre-known, for OptimalAction statistics
    ,_qValues :: [Double] -- estimate value of each action
    ,_nActions :: [Int] -- count of each atcion has taken
    ,_stepSize :: Double
    ,_totalReward :: Double
    ,_curStep :: Int
    ,_bestTakes :: [Double]
    -- input randoms
    ,_srcRVars :: [RVar Double]
    -- exploit & explore methods
    ,_policy :: Policy
    }

makeLenses ''Bandit

step :: StateT Bandit IO Double
step = selectAction >>= takeOneAction

selectAction :: StateT Bandit IO Int
selectAction = do
  bandit <- get
  actionN <- case _policy bandit of
    EGreedy epsilonRVar ->
      bExplore <- liftIO $ sample epsilonRVar            
        if bExplore then liftIO $ sample (randomElement [0..((_kArms bandit) - 1)])
           else pure . fst . argmaxWithIndex $ (zip [0..] (_qValues bandit))
    _ -> pure 0
 
takeOneAction :: Int -> StateT Bandit IO Double
takeOneAction = do
  bandit <- 
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

runLoop :: Int -> State Bandit (Double)
runLoop = replicateM (runState a 

runLoop :: Bandit a => a -> Int -> IO (Vector Double)
runLoop bandit steps = do
  updatedRef <- newIORef bandit
  averageRewards <- go 1 updatedRef bandit
  bandit' <- readIORef updatedRef
  -- print (_bestTakes bandit')
  pure $ LA.fromList (averageRewards ++ (_bestTakes bandit'))
  where
  go count updatedRef bandit
    | count > (_totalStep bandit) = writeIORef updatedRef bandit >> pure []
    | otherwise = do
        bExplore <- sample (_greedyEpsilonRVar bandit)
        newSA <- takeOneAction bExplore bandit
        ( (_totalReward newSA) / (fromIntegral count) :) <$> go (count + 1) updatedRef newSA
