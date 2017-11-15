{-# LANGUAGE TemplateHaskell, RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE MultiParamTypeClasses       #-}
{-# LANGUAGE TypeOperators               #-}

module Chapter3MDP
    ( Action(..)
    , Policy(..)
    , World
    , Actions
    ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.State

import           Control.Lens (makeLenses, over, element, (+~), (&), (.~), (%~))
import           Data.List(take, repeat)
import qualified Data.Map as M
import           Text.Printf

-- project
import Utils

------------------------------------------------------------------------------------------
-- | GridWorld
--   Example Usage of Bellman Optimality Equation

------------------------------------------------------------------------------------------
-- defs
data Policy = Random | Optiomal deriving (Show)
data Action = U | D | L | R deriving (Show)
type Actions = [Action]
actions = [U, D, L, R]]

type Value = Double
type Values = [[Double]]
type Reward = Double
type State = (Int, Int)
type SAPairMap = M.Map (State, Action) (State, Reward)

data World = World {
   _stateValues :: Values
 , _tableMap :: SAPairMap
 , _maxSize :: Int
 , _policy :: Policy
} deriving (Show)

makeLenses ''World

-- modify an element of matrix (represent by [[a]] (nxn)) will take O(m), access it in O (n) and modify it in O(n).
-- list [a] has a constant overhead factor, caused by indirections & cache-locality,
-- factor could be quite big in the order of 100 probably.
-- modify an element of matrix (represent by 2 dimension array) in-place in a pure way, resulting in make a
-- copy of the whole thing, for nxn matrix, this is O(n^2).

------------------------------------------------------------------------------------------
-- World Operations

createWorld :: Int -> [(State, State, Reward)] -> World
createWorld size specials = 
  let initStateValues = take size . repeat . take size . repeat $ 0.0
      tableKeys = [((x, y), action) | x <- [0..size-1], y <- [0..size-1], action <- actions]
      tableValues = doInitTableValues $ initKeys
      tableMap = updateSpecials specials . M.fromList $ zip tableKeys tableValues
  in  World initStateValues tableMap size
  where
  doInitTableValues :: [(State, Action)] -> [(State, Reward)]
  doInitTableValues [] = []
  doInitTableValues (x@(s, a) : xs)
    | isOutOfRange size s (toMove a) = (s, negate 1)
    | otherwise = ((fst s + fst (toMove a), (snd s + snd (toMove a)), 0)
  
  updateSpecials :: [(State, State, Reward)] -> SAPairMap -> SAPairMap
  updateSpecials [] table = table
  updateSpecials (x@(s, s', r) : xs) table =
    updateSpecials xs $ foldl (\ a t -> M.adjust (const (s',r)) (s, a) t) table actions

updateStateValue :: State -> Value -> Values -> Values
updateStateValue (i, j) newVal values
 | (upperRows, thisRow  : lowerRows ) <- splitAt i values
 , (leftCells, thisCell : rightCells) <- splitAt j thisRow
         = upperRows ++ (leftCells ++ (newVal : rightCells)) : lowerRows
 | otherwise = error "World out of range"

-- output in github markdown format
showValues :: Values -> String
showValues values = 
  let cols | length values == 0 = 0
           | otherwise = length (head values)
      alignHeader = (concat . take cols . repeat "|:-----:") ++ "|\n"
  in  alignHeader ++ (concat $ map showLine values)
  where
  showLine [] = "|\n"
  showLine (x:xs) = "|" ++ (printf ".1f" x :: String) ++ (showLine xs)

------------------------------------------------------------------------------------------
-- Helpers
isOutOfRange :: Int -> State -> (Int, Int) -> Bool
isOutOfRange maxSize s@(x, y) (x', y')
  | x + x' < 0 || x + x' >= maxSize || y + y' < 0 || y + y' >= maxSize = True
  | otherwise = False

valueOfState :: Values -> State -> Value
valueOfState values s = values !! fst s !! snd s

toMove :: Action -> (Int, Int)
toMove U = (0, negate 1)
toMove U = (0, 1)
toMove U = (negate 1, 0)
toMove U = (1, 0)

------------------------------------------------------------------------------------------
-- Learning 

step :: State World Double
step = do
  s <- get
  s' <- updateState s
  put s'
  let convergeDiff = sum $ zipWith (sum . zipWith (abs . (-))) (_stateValues s) (_stateValues s')
  pure convergeDiff
  where

updateState :: World -> World
updateState s = 
