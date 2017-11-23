{-# LANGUAGE TemplateHaskell, RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE MultiParamTypeClasses       #-}
{-# LANGUAGE TypeOperators               #-}
{-# LANGUAGE TypeSynonymInstances        #-}
{-# LANGUAGE FlexibleInstances           #-}

module Chapter4DP
    ( CarRental(..)
    , mkCarRental
    , step
    ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.State

import           Control.Lens (makeLenses, over, element, (+~), (&), (.~), (%~))
import           Data.List(take, repeat)
import           Data.Foldable (toList)
import           Data.Maybe

import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Random
import           Data.Random.Distribution
import           Data.Random.Distribution.Poisson

import           Language.Haskell.TH

-- project
import           Utils

------------------------------------------------------------------------------------------
-- | Dynamic Programming Experiments: Car Rental
--   States: number of cars in each location, represents as: Seq [Int]
--   Actions: number of cars Transferred between locations, represents as: Seq (Seq [[Int]])
--   Returns: rental credit, transfer cost, parking cost, additional saveings, represents as: double

------------------------------------------------------------------------------------------
instance Show (RVar Int) where
  show _ = "poisson random variables"

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
    ,_rentPoissonR :: [RVar Int]
    ,_returnPoissonR :: [RVar Int]
    -- state & action: using the same index
    ,_states :: Seq [Int]
    -- if < 0, using stochastic policy (try every action equprob) as initial policy
    ,_actions :: Seq Int
    ,_possibleActions :: Seq (Seq [[Int]]) -- transfer out for each location
    ,_stateValues :: Seq Double -- will do in place update (Seq.update at n)
    } deriving (Show)

makeLenses ''CarRental

mkCarRental :: Int -> Double -> Double -> [Int] -> [Double] -> [Double] -> [Int] ->
                      [Int] -> [Double] -> [Double] -> [RVar Int] -> [RVar Int] -> CarRental
mkCarRental nLocations theTheta gamma maxCarNums earns transCost maxTrans freeLimit parkFee savings rentR returnR =
  CarRental nLocations theTheta gamma maxCarNums earns transCost maxTrans freeLimit parkFee savings rentR returnR
            (Seq.fromList allStates) initActions (Seq.fromList $ map Seq.fromList possibleActions) stateVals
  where
  allStates = generateStates maxCarNums
  initActions = Seq.fromList . take (length allStates) $ repeat (-1)
  stateVals = Seq.fromList . take (length allStates) $ repeat 0.0
  possibleActions = filterPossibilities allStates maxCarNums $ generateMoves maxTrans

-- generate all states for multiple locations
generateStates :: [Int] -> [[Int]]
generateStates [] = [[]]
generateStates (x:xs) = [(y:ys) | y <- [0..x], ys <- generateStates xs]

-- generate one location's tansfer out possibilities
generateOneMove :: Int -> Int -> [[Int]]
generateOneMove 0   n = [[]]
generateOneMove len n = [(x:xs) | x <- [0..n-1], xs <- generateOneMove (len - 1) n, sum (x:xs) <= n]

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

--------------------------------------------------------------------------------
-- | learning: Policy Iteration Page-65 (Not In Place Update)
--             Initially use stochastic policy, then use greedy

step :: StateT CarRental IO (Bool, Int)
step = get >>= policyEvaluation >>= put >> get >>= policyImprovement

-- TODO: not take parking, saving into account
policyEvaluation :: CarRental -> StateT CarRental IO CarRental
policyEvaluation carRental = do
  let oldStateVals = _stateValues carRental
  newStateVals <- updateStateValues carRental oldStateVals
  let carRental' = carRental & (stateValues .~ newStateVals)
      maxDiff = argmax $ toList (Seq.zipWith (\ x y -> abs (x-y)) newStateVals oldStateVals)
  if maxDiff < (_theta carRental)
     then pure carRental'
     else policyEvaluation carRental'

updateStateValues :: CarRental -> Seq Double -> StateT CarRental IO (Seq Double)
updateStateValues carRental oldStateValues = do
  -- it is ok to use the same one
  rents <- liftIO $ mapM sample (_rentPoissonR carRental)
  returns <- liftIO $ mapM sample (_returnPoissonR carRental)
  let acts = _actions carRental
  case or $ fmap ( < 0) acts of
    True -> pure (Seq.zipWith (\ s as ->
                                 (foldl (+) 0.0
                                    (fmap (caclOneActionValue carRental rents returns s) as))
                                 / (fromIntegral $ length as)
                              )
                              (_states carRental) (_possibleActions carRental))
    False -> pure (Seq.zipWith3 (\ actN s as -> caclOneActionValue carRental
                                                    rents returns s (Seq.index as actN)
                                )
                                acts (_states carRental) (_possibleActions carRental))

-- state action pair (s, a) 
caclOneActionValue :: CarRental -> [Int] -> [Int] -> [Int] -> [[Int]] -> Double
caclOneActionValue carRental rents returns s a = 
  let oldStateValues = _stateValues carRental
      locationOut = map sum a
      locationIn = foldl (zipWith (+)) (take (_locationNum carRental) $ repeat 0) a
      transferFees = sum $ zipWith (*) (_transferCost carRental) (map fromIntegral locationOut)
      sNight = zipWith (-) s (zipWith (-) locationOut locationIn)
      rents' = zipWith (\ x y -> (x < y) ? (x, y)) sNight rents
      rentFees = sum $ zipWith (*) (_rentalCredit carRental) (map fromIntegral rents')
      sAfterRent = zipWith (-) s rents'
      sAfterReturn = zipWith (+) sAfterRent returns
      sFinal = zipWith (\ x y -> (x <= y) ? (x, y)) sAfterReturn (_maxCars carRental)
      finalStateIndex = fromJust . Seq.elemIndexL sFinal $ _states carRental
  in  (rentFees - transferFees) + (_discount carRental) * ((_stateValues carRental) `Seq.index` finalStateIndex)

-----------------------------------------------------------------------------------------
---- policy improvement, update policy
policyImprovement :: CarRental -> StateT CarRental IO (Bool, Int)
policyImprovement carRental = do
  let oldActions = _actions carRental
  rents <- liftIO $ mapM sample (_rentPoissonR carRental)
  returns <- liftIO $ mapM sample (_returnPoissonR carRental)
  let actionReturns = Seq.zipWith (\ s as -> fmap (caclOneActionValue carRental rents returns s) as)
                                  (_states carRental) (_possibleActions carRental) :: Seq (Seq Double)
      newActions = fmap (fst . argmaxWithIndex . zip [0..] . toList) actionReturns
      diffs = toList $ Seq.zipWith (-) oldActions newActions
      percent = round (  ((100.0 *) . fromIntegral . length $ filter (==0) diffs)
                       / (fromIntegral $ length newActions))
      carRental' = carRental & (actions .~ newActions)
  put carRental
  if percent >= 100
     then pure (True, 100)
     else pure (False, 0)

--------------------------------------------------------------------------------
