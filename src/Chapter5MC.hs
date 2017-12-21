{-# LANGUAGE TemplateHaskell, RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE MultiParamTypeClasses       #-}
{-# LANGUAGE TypeOperators               #-}
{-# LANGUAGE TypeSynonymInstances        #-}
{-# LANGUAGE FlexibleInstances           #-}

module Chapter5MC
    ( Blackjack(..)
    , BJAct(..)
    , Racetrack(..)
    , mkBlackjack
    , blackjackStep
    , mkRacetrack
    , racetrackStep
    ) where

import           Control.Monad
import           Control.Monad.ST
import           Control.Monad.IO.Class
import           Control.Monad.Trans.State

import           Control.Lens (makeLenses, over, element, (+~), (&), (.~), (%~))
import           Data.List(take, repeat)
import           Data.Foldable (toList)
import           Data.Maybe

import qualified Data.Map.Strict as M

import           Data.Random
import           Data.Random.Distribution
import           Data.Random.Distribution.Bernoulli (bernoulli)

import           Language.Haskell.TH

-- project
import           Debug.Trace
import           Utils

------------------------------------------------------------------------------------------
-- | Monte Carlo Method Experiment: Blackjack
--   Will implement: MonteCarlo-E-Soft (On-Policy)

data BJAct = Hit | Stick deriving (Show, Ord, Eq)
-- ((sum, dealer's one card, usable ace), act)
type SAPair = ((Int, Int, Bool), BJAct)

-- epsilon-soft, reduce epsilon as episode increase (epsilon <- 1 / episode count)
data Blackjack = Blackjack {
     _stepSize :: Double -- usually < 0, to use (1 / (s,a) occur count) as step size (stationary)
    ,_qValues :: M.Map SAPair (Int, Double) -- (state-action occurs count, sa value)
    } deriving (Show)

makeLenses ''Blackjack

mkBlackjack :: Double -> Blackjack
mkBlackjack stepSize =
  let sas = [((s, d, bAce), a) | s <- [12..21], d <- [1..10],
                                 bAce <- [True, False], a <- [Hit, Stick]]
  in  Blackjack stepSize (M.fromList $ zip sas (repeat (0, 0.0)))

--------------------------------------------------------------------------------
blackjackStep :: Int -> StateT Blackjack IO Blackjack
blackjackStep count = do
  blackjack <- get
  dealerCards <- liftIO $ replicateM 21 nextCard -- at most 21 cards
  playerCards <- liftIO $ replicateM 21 nextCard
  -- player first (explode first ...)  
  let (dealerSum, _) = getSumViaFixedPolicy 17 dealerCards
  trajectories <- liftIO $ generatePlayerTrajectory blackjack count (head dealerCards) (0,0) playerCards
  let (playerSum, _, _) = fst $ last trajectories
      !reward | playerSum < 0 = negate 1.0
              | playerSum > dealerSum = 1.0
              | playerSum == dealerSum = 0.0
              | otherwise = negate 1.0
      !trajectories' = filter (\ ((ps, _, _), _) -> ps >= 12 && ps <= 21) trajectories
      !blackjack' = foldl (updateValues reward) blackjack trajectories'
  put blackjack'
  pure blackjack'
  where
  updateValues reward blackjack sa =
    let oldValMap = _qValues blackjack
        ss = _stepSize blackjack
    in  blackjack {_qValues = M.adjust (go ss) sa oldValMap}
    where
    go ss (c, v) = if ss < 0 then (c+1, v + (1.0 / fromIntegral (c+1)) * (reward -v))
                      else (c+1, v + (ss * (reward -v)))

generatePlayerTrajectory :: Blackjack -> Int -> Int -> (Int, Int) -> [Int] -> IO [SAPair]
generatePlayerTrajectory _ _ _ _ [] = pure []
generatePlayerTrajectory blackjack count dfc (playerSum, aceNum) (x:xs)
  | playerSum < 11 && x == 1 = generatePlayerTrajectory blackjack count dfc (playerSum + 11, aceNum+ 1) xs
  | playerSum <= 11 = generatePlayerTrajectory blackjack count dfc (playerSum + x, aceNum) xs
  | playerSum == 21 = pure [((21, dfc, useAce aceNum), Stick)]
  | playerSum > 21 = if (aceNum > 0)
                        then generatePlayerTrajectory blackjack count dfc (playerSum - 10, aceNum - 1) (x:xs)
                        else pure [((negate 1, dfc, False), Stick)] -- blow up
  | playerSum < 21 = do
      a <- epsilonGreedyAct blackjack count (playerSum, dfc, useAce aceNum)
      case a of
        Stick -> pure [((playerSum, dfc, useAce aceNum), Stick)]
        Hit -> (((playerSum, dfc, useAce aceNum), Hit) :) <$>
                 generatePlayerTrajectory blackjack count dfc (playerSum + x, aceNum) xs

epsilonGreedyAct :: Blackjack -> Int -> (Int, Int, Bool) -> IO BJAct
epsilonGreedyAct blackjack count s = do
  bExplore <- headOrTail (1.0 / (fromIntegral count)) -- head means explore
  case bExplore of
    True -> headOrTail 0.5 >>= \ bHit -> pure (bHit ? (Hit, Stick)) -- head means hit
    False -> do
      let (_, hitVal) = fromJust $ M.lookup (s, Hit) (_qValues blackjack)
          (_, standVal) = fromJust $ M.lookup (s, Stick) (_qValues blackjack)
      pure ((hitVal > standVal) ? (Hit, Stick))
                       
--------------------------------------------------------------------------------
-- dealer policy: will stick when sum >= 17
getSumViaFixedPolicy :: Int -> [Int] -> (Int, Int)
getSumViaFixedPolicy standSum = foldl go (0, 0)
  where
  go (acc, aceAs11Num) card | acc < 0 = (acc, aceAs11Num) -- already blow up
                            | acc >= standSum = (acc, aceAs11Num) -- it is ok, just stick
                            | card == 1 && acc + 11 <= 21 = (acc + 11, aceAs11Num + 1)
                            | card == 1 && acc + 11 > 21 = (acc + 1, aceAs11Num)
                            | acc + card > 21 && aceAs11Num > 0 = (acc + card - 10, aceAs11Num - 1)
                            | acc + card > 21 = (negate 1, aceAs11Num) -- blow up
                            | otherwise = (acc + card, aceAs11Num)

--------------------------------------------------------------------------------
-- helpers
-- draws with replacement, for randomElement return pure RVar(no memory)
nextCard :: IO Int
nextCard = sample (randomElement [1..13]) >>= \ a -> pure (min a 10)

-- epsilon greedy, also random select Hit or Stick
headOrTail :: Double -> IO Bool
headOrTail eps = sample $ bernoulli eps

useAce :: Int -> Bool
useAce num = (num > 0) ? (True, False)

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
-- | Monte Carlo Method Experiment: Racetrack
--   Will implement: Monte Carlo Off-Policy

-- state: (position, horizon velocity, vertical velocity)
-- act:   (horizon velocity change, vertical velocity change)
type RTAct = (Int, Int)
type RTState = ((Int, Int), (Int, Int))
type RTSA = (RTState, RTAct)

data Racetrack = Racetrack {
     _world :: [[Int]] -- 0 startings, 1 ends, -1 boundaries
    ,_ww :: Int -- world's width
    ,_wh :: Int -- world's height
    ,_gamma :: Double
    ,_actFailProb :: Double -- there are chances action won't take effect
    ,_maxVelocity :: Int -- max velocity for both horizon & vertical velocity
    ,_bPolicy :: M.Map RTState [RTAct]
    ,_piPolicy :: M.Map RTState RTAct -- a deterministic policy
    ,_qValues :: M.Map RTSA Double
    ,_cValues :: M.Map RTSA Double
    } deriving (Show)

makeLenses'' Racetrack

mkRacetrack :: (Int, Int) -> Double -> Double -> Int -> Racetrack
mkRacetrack (w, h) discount actFailP maxV =
  let 
  in  Racetrack theWorld discount actFailP maxV behaviorP targetP qs cs

{- the world: '=' boundary, '.' starting, '|' is end
   it roughly has the following shape
   ===========..........====
   ==---------------------==
   =----------------------==
   =----------------------==
   =----------------------==
   ==---===---==---===-=====
   =======----------------==
   =======----------------==
   =======----------------==
   =======----------------==
   =========--------------==
   ===========------------==
   =============----------==
   ===============--------==
   ===============-------===
   ===============--------==
   ===============---------|
   ===============---------|
   ===============---------|
   ===============-------===
   ===============------====
   ===============------====
   ===============-----=====
   =========================
-}

genRaceWorld :: Int -> Int -> [[Int]]
genRaceWorld w h = do  
  let workldStart = over (elements (> (w `div` 2))) (const (negate 1)) .
                    over (elements (< (w `div` 2))) (const (negate 1)) . take w $ [0..]
