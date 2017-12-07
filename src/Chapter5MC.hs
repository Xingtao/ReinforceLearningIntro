{-# LANGUAGE TemplateHaskell, RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE MultiParamTypeClasses       #-}
{-# LANGUAGE TypeOperators               #-}
{-# LANGUAGE TypeSynonymInstances        #-}
{-# LANGUAGE FlexibleInstances           #-}

module Chapter5MC
    ( Blackjack(..)
    , mkBlackjack
    , blackjackStep
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
--   Will implement: MonteCarlo-E-Soft(On-Policy), MonteCarlo-Off-Policy

data Act = Hit | Stick deriving (Show, Ord, Eq)
-- ((sum, dealer's one card, usable ace), act)
type SAPair = ((Int, Int, Bool), Act)

-- epsilon-soft, reduce epsilon as episode increase
data Blackjack = Blackjack {
     _epsilon :: Double 
    ,_qValues :: M.Map SAPair Double
    } deriving (Show)

makeLenses ''Blackjack

mkBlackjack :: Double -> Blackjack
mkBlackjack epsilon =
  let sas = [((s, d, bAce), a) | s <- [12..21], d <- [1..10],
                                 bAce <- [True, False], a <- [Hit, Stick]]
  in  Blackjack epsilon (M.fromList $ zip sas (repeat 0.0))

--------------------------------------------------------------------------------
blackjackStep :: Int -> StateT Blackjack IO Blackjack
blackjackStep count = do
  blackjack <- get
  dealerCards <- liftIO $ replicateM 21 nextCard -- at most 21 cards
  playerCards <- liftIO $ replicateM 21 nextCard
  -- player first (explode first ...)  
  let (dealerSum, _) = getSumViaFixedPolicy 17 dealerCards
  trajectories <- liftIO $ generatePlayerTrajectory blackjack (head dealerCards) (0,0) playerCards
  let (playerSum, _, _) = fst $ last trajectories
      !reward | playerSum > dealerSum = 1.0
              | playerSum == dealerSum = 0.0
              | otherwise = negate 1.0
      !trajectories' = filter (\ ((ps, _, _), _) -> ps >= 12 && ps <= 21) trajectories
      !blackjack' = foldl (updateValues count reward) blackjack trajectories'
  put blackjack'
  pure blackjack'
  where
  updateValues count reward blackjack sa =
    let oldValMap = _qValues blackjack
    in  blackjack {_qValues = M.adjust (\v -> v + (1.0 / fromIntegral count) * (reward -v)) sa oldValMap}

generatePlayerTrajectory :: Blackjack -> Int -> (Int, Int) -> [Int] -> IO [SAPair]
generatePlayerTrajectory _ _ _ [] = pure []
generatePlayerTrajectory blackjack dfc (playerSum, aceNum) (x:xs)
  | playerSum < 11 && x == 1 = generatePlayerTrajectory blackjack dfc (playerSum + 11, aceNum + 1) xs
  | playerSum < 11 = generatePlayerTrajectory blackjack dfc (playerSum + x, aceNum) xs
  | playerSum == 11 && x == 1 = generatePlayerTrajectory blackjack dfc (12, aceNum) xs
  | playerSum == 11 = generatePlayerTrajectory blackjack dfc (11 + x, aceNum) xs
  | playerSum == 21 = pure [((21, dfc, useAce aceNum), Stick)]
  | playerSum > 21 = if (aceNum > 0)
                        then generatePlayerTrajectory blackjack dfc (playerSum - 10, aceNum - 1) xs
                        else pure [((playerSum, dfc, False), Stick)]
  | playerSum < 21 = do
      a <- epsilonGreedyAct blackjack (playerSum, dfc, useAce aceNum)
      case a of
        Stick -> pure [((playerSum, dfc, useAce aceNum), Stick)]
        Hit -> (((playerSum, dfc, useAce aceNum), Hit) :) <$>
                 generatePlayerTrajectory blackjack dfc (playerSum + x, aceNum) xs

epsilonGreedyAct :: Blackjack -> (Int, Int, Bool) -> IO Act
epsilonGreedyAct blackjack s = do
  bExplore <- headOrTail (_epsilon blackjack) -- head means explore
  case bExplore of
    True -> headOrTail 0.5 >>= \ bHit -> pure (bHit ? (Hit, Stick)) -- head means hit
    False -> do
      let !hitVal = fromJust $ M.lookup (s, Hit) (_qValues blackjack)
          !standVal = fromJust $ M.lookup (s, Stick) (_qValues blackjack)
      pure ((hitVal > standVal) ? (Hit, Stick))
                   
--------------------------------------------------------------------------------
-- policies: dealer will stand when sum >= 17; player is the one we try learning

getSumViaFixedPolicy :: Int -> [Int] -> (Int, Int)
getSumViaFixedPolicy standSum = foldl go (0, 0)
  where
  go (acc, aceAs11Num) card | acc >= standSum = (acc, aceAs11Num)
                            | card == 1 && acc + 11 <= 21 = (acc + 11, aceAs11Num + 1)
                            | card == 1 && acc + 11 > 21 = (acc + 1, aceAs11Num)
                            | acc + card > 21 && aceAs11Num > 0 = (acc + card - 10, aceAs11Num - 1)
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
