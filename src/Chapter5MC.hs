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
    , showBlackjackResult
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

data Act = Hit | Stand deriving (Show)
-- ((sum, dealer's one card, usable ace), act)
type SAPair = ((Int, Int, Bool), Act)

-- epsilon-soft 
data Blackjack = Blackjack {
     _epsilon :: Double 
    ,_sa :: M SAPair Int
    ,_qValues :: [Double]
    ,_saReturns :: [Double]
    ,_act :: [Act]
    } deriving (Show)

makeLenses ''Blackjack

mkBlackjack :: Double -> Blackjack
mkBlackjack epsilon =
  let sas = [((s, d, bAce), a) | s <- [12..21], d <- [1..10],
                                 bAce <- [True, False], a <- [Hit, Stand]]
      initQVals = take (length sas) (repeat 0.0)
      initSAReturns = take (length sas) (repeat 0.0)
  in  Blackjack epsilon (M.fromList $ zip sas [0..]) initQVals initSAReturns

--------------------------------------------------------------------------------
blackjackStep :: StateT Blackjack IO Blackjack
blackjackStep = get >>= oneEpisode >>= put
      
oneEpisode :: Blackjack -> StateT Blackjack IO Blackjack
oneEpisode blackjack = do
  dealerCards <- liftIO $ replicateM 21 nextCard
  playerCards <- liftIO $ replicateM 21 nextCard
  -- player first (explode first ...)  
  let (dealerSum, _) = getSumViaFixedPolicy 17 dealerCards  
  epsilonGreedyPlayOneEpisode blackjack playerCards (dealerSum, head dealerCards)
  
epsilonGreedyPlayOneEpisode :: Blackjack -> [Int] -> (Int, Int) -> StateT Blackjack IO Blackjack
epsilonGreedyPlayOneEpisode blackjack playerCards (dealerSum, dealerFirstCard) = do
  let qVals = _qValues blackjack
        

generateTrajectoryUsingEGreedy :: Blackjack -> playerCards -> 

  -- ((sum, dealer's one card, usable ace), hit/stand)
  type SAPair = ((Int, Int, Bool), Bool) 
  data Blackjack = Blackjack {
     _epsilon :: Double 
    ,_sa :: M SAPair Int
    ,_qValues :: [Double]
    } deriving (Show)


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

-- epsilon greedy, also random select Hit or Stand
headOrTail :: Double -> IO Bool
headOrTail eps = sample $ bernoulli eps
