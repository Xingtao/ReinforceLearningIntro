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

import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

import           Language.Haskell.TH

-- project
import           Debug.Trace
import           Utils

------------------------------------------------------------------------------------------
-- | Monte Carlo Method Experiment: Blackjack

data BJAct = Hit | Stand deriving (Show)

------------------------------------------------------------------------------------------
data Blackjack = Blackjack {
     _locationNum :: Int
    ,_theta :: Double
    ,_discount :: Double
    ,_maxCars :: [Int]
    ,_rentalCredit :: [Double]
    ,_transferCost :: [Double]
    ,_maxTransferCars :: [Int]
    ,_freeParkingLimit :: [Int]
    ,_additionalParkingCost :: [Double]
    ,_additionalTransferSaving :: [Double]
    -- input randoms
    ,_rentLambda :: [Double]
    ,_returnLambda :: [Double]
    -- state & action: using the same index
    ,_states :: Seq [Int]
    -- if < 0, using stochastic policy (try every action equprob) as initial policy
    ,_actions :: Seq Int
    ,_possibleActions :: Seq (Seq [[Int]]) -- transfer out for each location
    ,_stateValues :: Seq Double -- will do in place update (Seq.update at n)
    ,_rentPoissonR :: [[(Int, Double)]]
    ,_returnPoissonR :: [[(Int, Double)]]
    ,_jointRentR :: [(Double, [Int])]
    ,_jointReturnR :: [(Double, [Int])]
    } deriving (Show)

makeLenses ''Blackjack

mkBlackjack :: Int -> Double -> Double -> [Int] -> [Double] -> [Double] -> [Int] ->
                      [Int] -> [Double] -> [Double] -> [Double] -> [Double] -> Blackjack
mkBlackjack nLocations theTheta gamma maxCarNums earns
            transCost maxTrans freeLimit parkFee savings rent return =
  Blackjack nLocations theTheta gamma maxCarNums earns 
            transCost maxTrans freeLimit parkFee savings rent return
            (Seq.fromList allStates) initActions
            (Seq.fromList $ map Seq.fromList possibleActions) stateVals
            rentPoissonDist returnPoissonDist jointRentDist jointReturnDist
  where
  allStates = generateStates maxCarNums
  initActions = Seq.fromList . take (length allStates) $ repeat (-1)
  stateVals = Seq.fromList . take (length allStates) $ repeat 0.0
  possibleActions = filterPossibilities allStates maxCarNums $ generateMoves maxTrans
  prob lambda n = lambda ^ (fromIntegral n) / (fromIntegral $ factorial n)  * (exp $ negate lambda)
  -- NOTE: ignore poisson probability < 1% (take as 0)
  rentPoissonDist =
    zipWith (\ lambda maxN -> filter (\ (n, p) -> p > 0.001) .
                                map (\ n -> (n, prob lambda n)) $ [0..maxN]) rent maxCarNums 
  returnPoissonDist =
    zipWith (\ lambda maxN -> filter (\ (n, p) -> p > 0.001) .
                                map (\ n -> (n, prob lambda n)) $ [0..maxN]) return maxCarNums
  jointRentDist = map go (joint rentPoissonDist)
  jointReturnDist = map go (joint returnPoissonDist)
  go = foldl (\(p', v') (v, p) -> (p'*p, v' ++ [v])) (1.0, [])
  -- joint distributions over all locations
  joint :: [[(Int, Double)]] -> [[(Int, Double)]]
  joint [] = [[]]
  joint (x:xs) = [(y:ys) | y <- x, ys <- joint xs]

-- | Car Rental Helpers
showBlackjackResult :: Blackjack -> String
showBlackjackResult blackjack =
  concat . toList $ Seq.zipWith4
    (\ s actionIdx saPair val ->
       show s ++ " -> " ++ (show $ Seq.index saPair actionIdx) ++ " -> " ++ show val ++ "\n")
    (_states blackjack) (_actions blackjack) (_possibleActions blackjack) (_stateValues blackjack)

-- generate all states for multiple locations
generateStates :: [Int] -> [[Int]]
generateStates [] = [[]]
generateStates (x:xs) = [(y:ys) | y <- [0..x], ys <- generateStates xs]

-- generate one location's tansfer out possibilities
generateOneMove :: Int -> Int -> [[Int]]
generateOneMove 0   n = [[]]
generateOneMove len n = [(x:xs) | x <- [0..n], xs <- generateOneMove (len - 1) n, sum (x:xs) <= n]

-- combine all possible transfer moves
generateMoves :: [Int] -> [[[Int]]]
generateMoves trans = go moves
  where
  go [] = [[]]
  go (x:xs) = [(y:ys) | y <- x, ys <- go xs]
  moves = map (generateOneMove (length trans)) trans

-- filter impossible transfer moves
filterPossibilities :: [[Int]] -> [Int] -> [[[Int]]] -> [[[[Int]]]]
filterPossibilities [] _ _ = []
filterPossibilities (s:ss) maxCarNums possibleMoves =
  filter (go maxCarNums s) possibleMoves : filterPossibilities ss maxCarNums possibleMoves
  where
  go :: [Int] -> [Int] -> [[Int]] ->Bool
  go maxCarNums s move =
    let moveLocationNums = foldl (zipWith (+)) (take (length s) $ repeat 0) move
        c1 = and $ zipWith3 (\ x y z -> x + y <= z) s moveLocationNums maxCarNums
        c2 = and $ zipWith3 (\ index curNum m -> (m!!index == 0) && (sum m <= curNum)) [0..] s move
    in  c1 && c2

-- showSAPair

--------------------------------------------------------------------------------
-- | learning: Policy Iteration Page-65 (Not In Place Update)
--             Initially use stochastic policy, then use greedy

blackjackStep :: State Blackjack (Bool, Int)
blackjackStep = get >>= policyEvaluation >>= put >> get >>= policyImprovement

-- TODO: not take parking, saving into account
policyEvaluation :: Blackjack -> State Blackjack Blackjack
policyEvaluation blackjack = do
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
