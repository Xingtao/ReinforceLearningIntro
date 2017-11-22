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
import           Data.Sequence (Seq)
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
    ,_greedyAction :: Seq Int
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
  initActions = Seq.fromList . take (sum $ map (nLocations*) maxCarNums) $ repeat 0
  stateVals = Seq.fromList . take (sum $ map (nLocations*) maxCarNums) $ repeat 0.0
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
---- learning: Policy Iteration (In Place Update). Page-65

step :: StateT CarRental IO (Bool, Int)
step = policyEvaluation >> policyImprovement

policyEvaluation :: StateT CarRental IO Bool
policyEvaluation = do
  carRental <- get
  pure True

policyImprovement :: StateT CarRental IO (Bool, Int)
policyImprovement = do
  carRental <- get
  pure (True, 100)
--------------------------------------------------------------------------------
