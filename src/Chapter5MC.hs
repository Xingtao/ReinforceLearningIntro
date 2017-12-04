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
  let sas = [((s, d, a), bHit) <- s <- [12..21], d <- [1..10], a <- [True, False], bHit <- [True, False]]
      initQVals = take (length sas) (repeat 0.0)
      initSAReturns = take (length sas) (repeat 0.0)
  Blackjack epsilon (M.fromList $ zip sas [0..]) initQVals initSAReturns

-- helpers
-- draws with replacement, for randomElement return pure RVar(no memory)
nextCard :: IO Int
nextCard = sample (randomElement [1..13]) >>= \ a -> pure (min a 10)

--------------------------------------------------------------------------------
blackjackStep :: State Blackjack (Bool, Int)
blackjackStep = get >>= oneEpisode >>= put >> get >>= policyImprovement


oneEpisode :: Blackjack -> State Blackjack Blackjack
oneEpisode blackjack = do
  let oldStateVals = _stateValues blackjack
  newStateVals <- updateStateValues blackjack oldStateVals
  let !blackjack' = blackjack & (stateValues .~ newStateVals)
      !maxDiff = maximum $ toList (Seq.zipWith (\ x y -> abs (x-y)) newStateVals oldStateVals)
  if maxDiff < (_theta blackjack)
     then pure blackjack'
     else policyEvaluation blackjack'

updateStateValues :: Blackjack -> Seq Double -> State Blackjack (Seq Double)
updateStateValues blackjack oldStateValues = do
  let acts = _actions blackjack
  case or $ fmap ( < 0) acts of
    True -> pure (Seq.zipWith3 (\ stateIdx s as ->
                                  (foldl (+) 0.0
                                     (fmap (caclOneActionValue blackjack stateIdx s) as))
                                  / (fromIntegral $ length as)
                               )
                               (Seq.fromList [0..(length $ _states blackjack) - 1])
                               (_states blackjack) (_possibleActions blackjack))
    False -> pure (Seq.zipWith4 (\ stateIdx s as actN ->
                                   caclOneActionValue blackjack stateIdx s (Seq.index as actN))
                                (Seq.fromList [0..(length $ _states blackjack) - 1])
                                (_states blackjack) (_possibleActions blackjack) acts)

-- | state action pair (s, a) value: sum up on all possibilities
--   sum(s',r)(p (s', r | s, a)*[r + discount v(s')])
caclOneActionValue :: Blackjack -> Int -> [Int] -> [[Int]] -> Double
caclOneActionValue blackjack stateIdx s a =
  let jointRentDist = _jointRentR blackjack
      jointReturnDist = _jointReturnR blackjack
      -- jointAll = [(x, y) | x <- jointRentDist, y <- jointReturnDist]
  -- NOTE: Use constant returns to accelerate the computations.
  --       The results state values are almost the same
  --in  sum (fmap (\ ((p1, rents), (p2, returns)) ->
  --                  p1*p2*(calcOneActionTransition blackjack stateIdx s a rents returns)
  --              ) jointAll)
  in  sum (fmap (\ (p1, rents) -> p1 * (calcOneActionTransition blackjack stateIdx s a
                                              rents (map round $ _returnLambda blackjack))
                ) jointRentDist)

calcOneActionTransition :: Blackjack -> Int -> [Int] -> [[Int]] -> [Int] -> [Int] -> Double
calcOneActionTransition blackjack stateIdx s a rents returns =
  let locationsOut = map sum a
      locationsIn = foldl (zipWith (+)) (take (_locationNum blackjack) $ repeat 0) a
      transferFees = sum $ zipWith (*) (_transferCost blackjack) (map fromIntegral locationsOut)
      sNight = zipWith (+) locationsIn (zipWith (-) s locationsOut)
      sNight' = minElement sNight (_maxCars blackjack)
      -- the second day
      rents' = minElement sNight' rents
      rentIncomes = sum $ zipWith (*) (_rentalCredit blackjack) (map fromIntegral rents')
      sAfterRent = zipWith (-) sNight' rents'
      sAfterReturn = zipWith (+) sAfterRent returns
      sFinal = minElement sAfterReturn (_maxCars blackjack)
      !finalStateIndex = fromJust (Seq.elemIndexL sFinal $ _states blackjack)
      base = (rentIncomes - transferFees) +
             (_discount blackjack) * ((_stateValues blackjack) `Seq.index` finalStateIndex)
      parkingFee = sum $ zipWith3 (\ s limit fee -> (s > limit) ? (fee, 0))
                                  sFinal (_freeParkingLimit blackjack) (_additionalParkingCost blackjack)
      transferSaving = sum $ zipWith (\ transfers saving -> (sum transfers > 0) ? (saving, 0))
                                     a (_additionalTransferSaving blackjack)      
  in  base - parkingFee + transferSaving

-----------------------------------------------------------------------------------------
---- policy improvement, update policy
policyImprovement :: Blackjack -> State Blackjack (Bool, Int)
policyImprovement blackjack = do
  let oldActions = _actions blackjack
      actionReturns =
        Seq.zipWith3 (\ idx s as -> fmap (caclOneActionValue blackjack idx s) as)
                     (Seq.fromList [0..(length $ _states blackjack) - 1])
                     (_states blackjack) (_possibleActions blackjack)
      newActions = fmap (fst . argmaxWithIndex . zip [0..] . toList) actionReturns
      diffs = toList $ Seq.zipWith (-) oldActions newActions
      percent = round (  ((100.0 *) . fromIntegral . length $ filter (== 0) diffs)
                       / (fromIntegral $ length newActions))
      !zero = trace ("=======> percent: " ++ show percent) 0 
      blackjack' = blackjack & (actions .~ newActions)
  put blackjack'
  if percent >= 100
     then pure (True, 100)
     else pure (False, percent)
