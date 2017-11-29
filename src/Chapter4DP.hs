{-# LANGUAGE TemplateHaskell, RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE MultiParamTypeClasses       #-}
{-# LANGUAGE TypeOperators               #-}
{-# LANGUAGE TypeSynonymInstances        #-}
{-# LANGUAGE FlexibleInstances           #-}

module Chapter4DP
    ( CarRental(..)
    , mkCarRental
    , carRentalStep
    , showCarRentalResult
    , Gambler(..)
    , mkGambler
    , gamblerStep
    , showGamblerResult
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
-- | Dynamic Programming Experiments: Car Rental
--   States: number of cars in each location, represents as: Seq [Int]
--   Actions: number of cars Transferred between locations, represents as: Seq (Seq [[Int]])
--   Returns: rental credit, transfer cost, parking cost, additional saveings, represents as: double

------------------------------------------------------------------------------------------
data CarRental = CarRental {
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

makeLenses ''CarRental

mkCarRental :: Int -> Double -> Double -> [Int] -> [Double] -> [Double] -> [Int] ->
                      [Int] -> [Double] -> [Double] -> [Double] -> [Double] -> CarRental
mkCarRental nLocations theTheta gamma maxCarNums earns
            transCost maxTrans freeLimit parkFee savings rent return =
  CarRental nLocations theTheta gamma maxCarNums earns 
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
showCarRentalResult :: CarRental -> String
showCarRentalResult carRental =
  concat . toList $ Seq.zipWith4
    (\ s actionIdx saPair val ->
       show s ++ " -> " ++ (show $ Seq.index saPair actionIdx) ++ " -> " ++ show val ++ "\n")
    (_states carRental) (_actions carRental) (_possibleActions carRental) (_stateValues carRental)

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

carRentalStep :: State CarRental (Bool, Int)
carRentalStep = get >>= policyEvaluation >>= put >> get >>= policyImprovement

-- TODO: not take parking, saving into account
policyEvaluation :: CarRental -> State CarRental CarRental
policyEvaluation carRental = do
  let oldStateVals = _stateValues carRental
  newStateVals <- updateStateValues carRental oldStateVals
  let !carRental' = carRental & (stateValues .~ newStateVals)
      !maxDiff = maximum $ toList (Seq.zipWith (\ x y -> abs (x-y)) newStateVals oldStateVals)
  if maxDiff < (_theta carRental)
     then pure carRental'
     else policyEvaluation carRental'

updateStateValues :: CarRental -> Seq Double -> State CarRental (Seq Double)
updateStateValues carRental oldStateValues = do
  let acts = _actions carRental
  case or $ fmap ( < 0) acts of
    True -> pure (Seq.zipWith3 (\ stateIdx s as ->
                                  (foldl (+) 0.0
                                     (fmap (caclOneActionValue carRental stateIdx s) as))
                                  / (fromIntegral $ length as)
                               )
                               (Seq.fromList [0..(length $ _states carRental) - 1])
                               (_states carRental) (_possibleActions carRental))
    False -> pure (Seq.zipWith4 (\ stateIdx s as actN ->
                                   caclOneActionValue carRental stateIdx s (Seq.index as actN))
                                (Seq.fromList [0..(length $ _states carRental) - 1])
                                (_states carRental) (_possibleActions carRental) acts)

-- | state action pair (s, a) value: sum up on all possibilities
--   sum(s',r)(p (s', r | s, a)*[r + discount v(s')])
caclOneActionValue :: CarRental -> Int -> [Int] -> [[Int]] -> Double
caclOneActionValue carRental stateIdx s a =
  let jointRentDist = _jointRentR carRental
      jointReturnDist = _jointReturnR carRental
      -- jointAll = [(x, y) | x <- jointRentDist, y <- jointReturnDist]
  -- NOTE: Use constant returns to accelerate the computations.
  --       The results state values are almost the same
  --in  sum (fmap (\ ((p1, rents), (p2, returns)) ->
  --                  p1*p2*(calcOneActionTransition carRental stateIdx s a rents returns)
  --              ) jointAll)
  in  sum (fmap (\ (p1, rents) -> p1 * (calcOneActionTransition carRental stateIdx s a
                                              rents (map round $ _returnLambda carRental))
                ) jointRentDist)

calcOneActionTransition :: CarRental -> Int -> [Int] -> [[Int]] -> [Int] -> [Int] -> Double
calcOneActionTransition carRental stateIdx s a rents returns =
  let locationsOut = map sum a
      locationsIn = foldl (zipWith (+)) (take (_locationNum carRental) $ repeat 0) a
      transferFees = sum $ zipWith (*) (_transferCost carRental) (map fromIntegral locationsOut)
      sNight = zipWith (+) locationsIn (zipWith (-) s locationsOut)
      sNight' = minElement sNight (_maxCars carRental)
      -- the second day
      rents' = minElement sNight' rents
      rentIncomes = sum $ zipWith (*) (_rentalCredit carRental) (map fromIntegral rents')
      sAfterRent = zipWith (-) sNight' rents'
      sAfterReturn = zipWith (+) sAfterRent returns
      sFinal = minElement sAfterReturn (_maxCars carRental)
      !finalStateIndex = fromJust (Seq.elemIndexL sFinal $ _states carRental)
      base = (rentIncomes - transferFees) +
             (_discount carRental) * ((_stateValues carRental) `Seq.index` finalStateIndex)
      parkingFee = sum $ zipWith3 (\ s limit fee -> (s > limit) ? (fee, 0))
                                  sFinal (_freeParkingLimit carRental) (_additionalParkingCost carRental)
      transferSaving = sum $ zipWith (\ transfers saving -> (sum transfers > 0) ? (saving, 0))
                                     a (_additionalTransferSaving carRental)      
  in  base - parkingFee + transferSaving

-----------------------------------------------------------------------------------------
---- policy improvement, update policy
policyImprovement :: CarRental -> State CarRental (Bool, Int)
policyImprovement carRental = do
  let oldActions = _actions carRental
      actionReturns =
        Seq.zipWith3 (\ idx s as -> fmap (caclOneActionValue carRental idx s) as)
                     (Seq.fromList [0..(length $ _states carRental) - 1])
                     (_states carRental) (_possibleActions carRental)
      newActions = fmap (fst . argmaxWithIndex . zip [0..] . toList) actionReturns
      diffs = toList $ Seq.zipWith (-) oldActions newActions
      percent = round (  ((100.0 *) . fromIntegral . length $ filter (== 0) diffs)
                       / (fromIntegral $ length newActions))
      !zero = trace ("=======> percent: " ++ show percent) 0 
      carRental' = carRental & (actions .~ newActions)
  put carRental'
  if percent >= 100
     then pure (True, 100)
     else pure (False, percent)

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
-- Gambler Problem
--   undiscounted, episodeic finite MDP, reward 1 when reaches goal, otherwise 0

data Gambler = Gambler {
    headProb :: Double
  , thetaCriterion :: Double
  , goal :: Int
  , stateVals :: VU.Vector Double
  , stateAct :: VU.Vector Int
  } deriving (Show)
  
showGamblerResult :: Gambler -> String
showGamblerResult = show

mkGambler :: Double -> Double -> Int -> Gambler
mkGambler prob theTheta gamblerGoal = 
  Gambler prob theTheta gamblerGoal
          (VU.fromList (take gamblerGoal (repeat 0.0) ++ [1.0])) 
          (VU.fromList (take gamblerGoal $ repeat 0))

------------------------------------------------------------------------------------------

gamblerStep :: State Gambler ()
gamblerStep = get >>= gamblerValueIteration >>= put

-- in-place update, convert to mutable array
gamblerValueIteration :: Gambler -> State Gambler Gambler
gamblerValueIteration gambler = do
  let gambler' = runValueIteration gambler
  put gambler' >> pure gambler'
    
runValueIteration :: Gambler -> Gambler
runValueIteration gambler = runST $ do
  mutStates <- VU.thaw (stateVals gambler)
  mutActs <- VU.thaw (stateAct gambler)
  (mutStates', mutActs') <- foldM evalAndImprove (mutStates, mutActs) [1..(goal gambler - 1)]
  newState <- VU.freeze mutStates'
  newActs <- VU.freeze mutActs'
  let !maxDiff = VU.maximum $ VU.zipWith ((abs .) . (-)) (stateVals gambler) newState
      gambler' = gambler {stateVals = newState, stateAct = newActs}
  if maxDiff < thetaCriterion gambler
     then pure gambler'
     else pure $ runValueIteration gambler'
  where
  evalAndImprove (mutStateVals, mutActVlas) index = do
    let possibleActions = [0..(min index (goal gambler - index))]
    returns <- mapM (\ act -> do
                       winVal <- VUM.read mutStateVals (index + act)
                       loseVal <- VUM.read mutStateVals (index - act)
                       pure $ (headProb gambler) * winVal + (1.0 - headProb gambler) * loseVal
                    ) possibleActions
    let (a, v) = argmaxWithIndex (zip [0..] returns)
    VUM.write mutStateVals index v
    VUM.write mutActVlas index a
    pure (mutStateVals, mutActVlas)  
