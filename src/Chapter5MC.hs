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
import           Data.Random.Distribution.Bernoulli

import           Language.Haskell.TH

-- project
import           Debug.Trace
import           Utils

------------------------------------------------------------------------------------------
-- | Monte Carlo Method Experiment: Blackjack
--   Will implement: MonteCarlo-E-Soft(On-Policy), MonteCarlo-Off-Policy

-- ((sum, dealer's one card, usable ace), hit/stand)
type SAPair = ((Int, Int, Bool), Bool)

------------------------------------------------------------------------------------------
-- epsilon-soft 
data Blackjack = Blackjack {
     _epsilon :: Double 
    ,_sa :: M SAPair Int
    ,_qValues :: [Double]
    ,_saReturns :: [Double]
    ,_policy :: [Bool] -- True: hit, False: stand
    } deriving (Show)

makeLenses ''Blackjack

mkBlackjack :: Double -> Blackjack
mkBlackjack epsilon =
  let sas = [((s, d, a), bHit) | s <- [12..21], d <- [1..10],
                                 a <- [True, False], bHit <- [True, False]]
      initQVals = take (length sas) (repeat 0.0)
      initSAReturns = take (length sas) (repeat 0.0)
  Blackjack epsilon (M.fromList $ zip sas [0..]) initQVals initSAReturns

-- helpers
-- draws with replacement, for randomElement return pure RVar(no memory)
nextCard :: IO Int
nextCard = sample (randomElement [1..13]) >>= \ a -> pure (min a 10)

--------------------------------------------------------------------------------
blackjackStep :: StateT Blackjack IO Blackjack
blackjackStep = get >>= oneEpisode >>= put
      
oneEpisode :: Blackjack -> StateT Blackjack IO Blackjack
oneEpisode blackjack = do
  dealerAppearCard <- liftIO $ nextCard
  
  type SAPair = ((Int, Int, Bool), Bool) 
  data Blackjack = Blackjack {
     _epsilon :: Double 
    ,_sa :: M SAPair Int
    ,_qValues :: [Double]
    ,_saReturns :: [Double]
    ,_policy :: [Bool] -- True: hit, False: stand
    } deriving (Show)
