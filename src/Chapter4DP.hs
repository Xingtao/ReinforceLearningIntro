{-# LANGUAGE TemplateHaskell, RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE MultiParamTypeClasses       #-}
{-# LANGUAGE TypeOperators               #-}

module Chapter4Bandit
    ( 
    , loopSteps
    ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.State
                  
import           Control.Lens (makeLenses, over, element, (+~), (&), (.~), (%~))
import           Data.List(take, repeat)
import qualified Data.Sequence as Seq
import qualified Data.Map as M
                  
import           Data.Random
import           Data.Random.Distribution
import           Data.Random.Distribution.Poisson

import           Language.Haskell.TH                  
import           Numeric.LinearAlgebra (Vector, Matrix)
import qualified Numeric.LinearAlgebra as LA

-- project
import           Utils

------------------------------------------------------------------------------------------
-- | Dynamic Programming Experiments: Car Rental & Gamblers Problem
--   States: Number of cars in each location, represent
--   Actions: Number of cars Transferred between locations
--   Returns: rental credit, transfer cost, parking cost, additional saveings

------------------------------------------------------------------------------------------
data CarRental = CarRental {
     _locationNum :: Int
    ,_discount :: Double
    ,_maxCars :: [Int]
    ,_rentalCredit :: [Double]
    ,_transferCost :: [Double]
    ,_maxTransferCars :: [Double]
    ,_freeParkingLimit :: [Int]
    ,_additionalParkingCost :: [Double]
    ,_additionalTransferSaving :: [Double]
    -- input randoms
    ,_rentPoissonR :: [RVar Int]
    ,_returnPoissonR :: [RVar Int]
    -- state & action: using the same index
    ,_states :: Seq [Int]
    ,_greedyAction :: Seq Int
    ,_possibleActions :: Seq [[Int]] -- transfer out
    ,_stateValues :: Seq Double -- will do in place update (Seq.update at n)
    }

makeLenses ''CarRental

mkCarRental :: Int -> Double -> [Int] -> [Double] -> [Double] ->
                      [Int] -> [Double] -> [Double] -> [RVar Int] -> [RVar Int] -> CarRental
mkCarRental nLocations gamma maxCarNums earns maxTrans freeLimit parkFee savings rentR returnR = 
  CarRental nLocations gamma maxCarNums earns maxTrans freeLimit parkFee savings rentR returnR
            (Seq.fromList allStates) initActions (Seq.fromList possibleActions) stateVals
  where
  allStates = generateStates maxCarNums
  initActions = Seq.fromList . take (n*max) $ repeat 0
  stateVals = Seq.fromList . take (n*max) $ repeat 0.0
  possibleActions = filterPossibles allStates maxCarNums $ generateMoves maxTrans

-- generate all states for multiple locations
generateStates :: [Int] -> [[Int]]
generateStates [] = [[]]
generateStates (x:xs) = [(y:ys) | y <- [0..x], ys <- generateKey xs]

-- generate one location's tansfer out possibilities
generateOneMove :: Int -> Int -> [[Int]]
generateOneMove 0   n = [[]]
generateOneMove len n = [(x:xs) | x <- [0..n-1], xs <- generateOneMove (len - 1) n, sum (x:xs) <= n]

generateMoves :: [Int] -> [[[Int]]]
generateMoves trans = go moves 
  where
  moves = map (generateOneMove (length trans)) trans


-- generate all actions for a given state
filterPossibles :: [[Int]] -> [Int] -> [[Int]] -> [[[Int]]]
filterPossibles [] _ _ = []
filterPossibles (x:xs) maxCarNums possibleMoves =
  filter (go s maxCarNums) possibleMoves : filterPossibles xs maxCarNums possibleMoves
  where
  -- the same len of the three argments
  go :: [Int] -> [Int] -> [Int] -> Bool
  go s maxCarNums move = 
  go (x:xs) = [(y:ys) | y <-[0..x], ys <- go xs]


--------------------------------------------------------------------------------
---- learning: Policy Iteration: In Place Update

loopSteps :: Int -> StateT CarRental IO [Double]
loopSteps times = replicateM times step

step :: StateT CarRental IO Double
step = selectOneAction >>= takeOneAction

selectOneAction :: StateT CarRental IO Int
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
  
takeOneAction :: Int -> StateT CarRental IO Double
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
  stateUpdate :: CarRental -> Int -> Double -> CarRental
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
