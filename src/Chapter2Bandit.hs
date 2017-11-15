{-# LANGUAGE TemplateHaskell, RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE MultiParamTypeClasses       #-}
{-# LANGUAGE TypeOperators               #-}

module Chapter2Bandit
    ( Bandit(..)
    , Policy(..)
    , mkBandit
    , loopSteps
    ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.State
                  
import           Control.Lens (makeLenses, over, element, (+~), (&), (.~), (%~))
import           Data.List(take, repeat)
                  
import           Data.Random
import           Data.Random.Distribution
import           Data.Random.Distribution.Uniform
import           Data.Random.Shuffle.Weighted                  
import           Data.Random.Source.IO

import           Language.Haskell.TH                  
import           Numeric.LinearAlgebra (Vector, Matrix)
import qualified Numeric.LinearAlgebra as LA

-- project
import           Utils

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
--      here, 'stepSize' parameter in configuration file:
--        if < 0, use 'sample average', then it is stationary;
--        if > 0, use 'exponential, recency-weighted average',
--                then it's non-stationary, weight recent reward more.
--   2. Gradient bandit using gradient approximation, estimate value as preference:
--           newValue = oldValue + stepSize * (curReward - baselineVal) * ( 1 - softmax)
--           for the selected action's state value update
--       and newValue = oldValue - stepSize * (curReward - baselineVal) * softmax
--           for all other non-selected actions'
--       where 'baseline' could be 0, or the average value of all action rewards until the time
--       and 'stepSize' is the same as above.

------------------------------------------------------------------------------------------
data Policy = EGreedy (RVar Bool) -- Bernoulli distribution with p = epsilon
            | UCB Double -- ucb explore parameter 'c'
            | Gradient Bool -- gradient baseline param, if < 0, use average reward value

data Bandit = Bandit {
     _kArms :: Int
    ,_bestValueIdx :: Int -- it is pre-known, for OptimalAction statistics
    ,_qValues :: ![Double] -- estimate value of each action
    ,_nActions :: ![Int] -- count of each atcion has taken
    ,_stepSize :: Double
    ,_totalReward :: !Double
    ,_curStep :: !Int
    ,_bestTakes :: ![Double]
    -- input randoms
    ,_srcRVars :: ![RVar Double]
    -- exploit & explore methods
    ,_policy :: Policy
    }

makeLenses ''Bandit

mkBandit :: Int -> Int -> Double -> Double -> [Double] -> Policy -> Bandit
mkBandit karm totalStep initValue stepSize trueValues policy = 
  let (maxValueIdx, _) = argmaxWithIndex (zip [0..] trueValues)
  in  (Bandit karm maxValueIdx (take karm $ repeat initValue) 
              (take karm $ repeat 0) stepSize 0.0 0 []
              (map (flip normal 1.0) trueValues) policy
      )

--------------------------------------------------------------------------------

loopSteps :: Int -> StateT Bandit IO [Double]
loopSteps times = replicateM times step

step :: StateT Bandit IO Double
step = selectOneAction >>= takeOneAction

selectOneAction :: StateT Bandit IO Int
selectOneAction = do
  bandit <- get
  actionN <- case _policy bandit of
    EGreedy epsilonRVar -> do
      bExplore <- liftIO $ sample epsilonRVar            
      if bExplore then liftIO $ sample (randomElement [0..((_kArms bandit) - 1)])
         else pure . fst . argmaxWithIndex $ (zip [0..] (_qValues bandit))
    UCB c -> do
      let !ucbEstValues = zipWith (calcUCB (_curStep bandit)) (_nActions bandit) (_qValues bandit)
      pure $ fst $ argmaxWithIndex (zip [0..] ucbEstValues)
    Gradient _ -> do
      let gradientValues = map exp (_qValues bandit)
          gradientProbs = map ( / sum gradientValues) gradientValues
          -- sampling according to distribution (element probability)
          -- the following line will produce a very good result, even 0.4 without baseline. why?
          -- is it because we do weighted random extract twice ?
          weightedRVar = weightedShuffle $ zip gradientProbs [0..]
      liftIO $ head <$> sample weightedRVar
      -- pure $ fst $ argmaxWithIndex (zip [0..] gradientProbs)
  pure actionN
  where
  calcUCB :: Int -> Int -> Double -> Double
  calcUCB total n val = val + sqrt ((log $ fromIntegral total) / fromIntegral n)
  
takeOneAction :: Int -> StateT Bandit IO Double
takeOneAction actionN = do
  bandit <- get
  reward <- liftIO $ sample (_srcRVars bandit !! actionN)
  let bestTake = (_bestValueIdx bandit == actionN) ? (1.0, 0.0)
      bandit' = bandit & (nActions . element actionN +~ 1)
                       & (totalReward +~ reward)
                       & (curStep +~ 1)
                       & (bestTakes %~ (++ [bestTake]))
      bandit'' = stateUpdate bandit' actionN reward      
  put bandit''
  pure reward
  where
  stateUpdate :: Bandit -> Int -> Double -> Bandit
  stateUpdate bandit actionN reward =
    let actionTakes = (_nActions bandit) !! actionN
        ss = (_stepSize bandit < 0) ? (1.0 / fromIntegral actionTakes, _stepSize bandit)
        oldEstValue = (_qValues bandit) !! actionN
    in  case _policy bandit of
          Gradient bBaseline ->
            let baseline = bBaseline ? (_totalReward bandit / (fromIntegral $ _curStep bandit), 0.0)
                gradientValues = map exp (_qValues bandit)
                gradientProbs = map ( / sum gradientValues) gradientValues
                newEstValues = zipWith3 (updateGradientPreference reward baseline ss)
                                        (_qValues bandit) gradientProbs [0..]
            in  bandit & (qValues .~ newEstValues)
          -- epsilone greedy & ucb use the same updating formular
          _ -> let newEstValue = oldEstValue + (reward - oldEstValue) * ss
               in  bandit & (qValues . element actionN .~ newEstValue)
  
  updateGradientPreference :: Double -> Double -> Double -> Double -> Double -> Int -> Double 
  updateGradientPreference reward baseline ss oldVal prob actionIdx =
    (actionN == actionIdx) ? (oldVal + ss * (reward - baseline) * (1 - prob),
                              oldVal - ss * (reward - baseline) * prob)
--------------------------------------------------------------------------------
