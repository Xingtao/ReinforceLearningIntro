{-# LANGUAGE TemplateHaskell, RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE MultiParamTypeClasses       #-}
{-# LANGUAGE TypeOperators               #-}
{-# LANGUAGE BangPatterns                #-}

module Chapter3MDP
    ( World(..)
    , Policy(..)
    , Action(..)
    , createWorld
    , step
    , showStateValues
    ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.State

import           Control.Lens (makeLenses, over, element, (+~), (&), (.~), (%~))
import           Data.List(take, repeat, sortOn)
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Text.Printf

-- project
import Utils

------------------------------------------------------------------------------------------
-- | GridWorld
--   Example Usage of Bellman Optimality Equation

------------------------------------------------------------------------------------------
-- defs
data Policy = PolicyRandom | PolicyOptimal deriving (Show)
data Action = U | D | L | R deriving (Show, Ord, Eq)
actions = [U, D, L, R]

type Values = [Double]
type Reward = Double
type StateCoor = (Int, Int)
type SAPairMap = M.Map (StateCoor, Action) (StateCoor, Reward)

data World = World {
    _stateValues :: Values
  , _tableMap :: SAPairMap
  , _maxSize :: Int
  , _policy :: Policy
  , _discount :: Double
} deriving (Show)

makeLenses ''World

instance Read Policy where
  readsPrec _ "random" = [(PolicyRandom, "")]
  readsPrec _ "optimal" = [(PolicyOptimal, "")]
  readsPrec _ _ = error "Unknown policy"

-- modify an element of matrix (represent by [[a]] (nxn)) will take O(m), access it in O (n) and modify it in O(n).
-- list [a] has a constant overhead factor, caused by indirections & cache-locality,
-- factor could be quite big in the order of 100 probably.
-- modify an element of matrix (represent by 2 dimension array) in-place in a pure way, resulting in make a
-- copy of the whole thing, for nxn matrix, this is O(n^2).

------------------------------------------------------------------------------------------
-- World Operations

createWorld :: Int -> Policy -> Double -> [(StateCoor, StateCoor, Reward)] -> World
createWorld size p gamma specials = 
  let initStateValues = take (size*size) . repeat $ 0.0
      tableKeys = [((x, y), action) | x <- [0..size-1], y <- [0..size-1], action <- actions]
      tableValues = doInitTableMap tableKeys
      tableMap = updateSpecials specials . M.fromList $ zip tableKeys tableValues
  in  World initStateValues tableMap size p gamma
  where
  doInitTableMap :: [(StateCoor, Action)] -> [(StateCoor, Reward)]
  doInitTableMap [] = []
  doInitTableMap (x@(s, a) : xs)
    | isOutOfRange size s (toMove a) = (s, negate 1) : doInitTableMap xs
    | otherwise = ((fst s + fst (toMove a), (snd s + snd (toMove a))), 0) : doInitTableMap xs
  
  updateSpecials :: [(StateCoor, StateCoor, Reward)] -> SAPairMap -> SAPairMap
  updateSpecials [] table = table
  updateSpecials (x@(s, s', r) : xs) table =
    updateSpecials xs $ foldl (\ t a -> M.adjust (const (s', r)) (s, a) t) table actions

-- output in github markdown format
showStateValues :: Int -> Values -> String
showStateValues size values = 
  let header = (concat . take size $ repeat "| ") ++ "|\n"      
      alignHeader = (concat . take size $ repeat "|:-----:") ++ "|\n"      
  in  header ++ alignHeader ++ (showRows (splitAt size values))
  where
  showRows ([], _) = "|\n"
  showRows (row, others) =
    (concat $ map (\ x -> "|" ++ (printf "%7.2f" x :: String)) row) ++ "|\n" ++ 
             (showRows $ splitAt size others)

------------------------------------------------------------------------------------------
-- Helpers
isOutOfRange :: Int -> StateCoor -> (Int, Int) -> Bool
isOutOfRange maxSize s@(x, y) (x', y')
  | x + x' < 0 || x + x' >= maxSize || y + y' < 0 || y + y' >= maxSize = True
  | otherwise = False

valueOfState :: Values -> StateCoor -> Int -> Double
valueOfState values s size = values !! (snd s * size + fst s)

-- not used
setValueOfState :: Values -> StateCoor -> Int -> Double -> Values
setValueOfState values s size val =
  let (lefts, rights) = splitAt (snd s * size + fst s) values
  in  lefts ++ [val] ++ tail rights

toMove :: Action -> (Int, Int)
toMove U = (0, negate 1)
toMove D = (0, 1)
toMove L = (negate 1, 0)
toMove R = (1, 0)

------------------------------------------------------------------------------------------
-- Learning 

step :: State World Double
step = do
  w <- get
  let w' = updateState w
  put w'
  let convergeDiff = maximum $ zipWith ((abs .) . (-)) (_stateValues w) (_stateValues w')
  pure convergeDiff

updateState :: World -> World
updateState w =
  let size = _maxSize w
      table = _tableMap w
      values = _stateValues w
      stateIdxs = [(x, y) | x <- [0..size-1], y <- [0..size-1]]
      stateIdxs' = sortOn snd stateIdxs
      updateValues = map (go table values size) stateIdxs'
  in  w {_stateValues = updateValues}
  where
  go :: SAPairMap -> Values -> Int -> StateCoor -> Double
  go table values size s =
    case _policy w of
      PolicyRandom -> -- Bellman Equation
        sum $ map (\ a -> let (s', r) = fromJust $ M.lookup (s, a) table
                          in  0.25 * (r + (_discount w) * (valueOfState values s' size))
                  ) actions
      PolicyOptimal -> -- Bellman Optimality Equation
        maximum $ map (\ a -> let (s', r) = fromJust $ M.lookup (s, a) table
                              in  r + (_discount w) * (valueOfState values s' size)
                      ) actions
