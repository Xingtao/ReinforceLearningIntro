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
data Action = U | D | L | R deriving (Show)
data Policy = Random | Optiomal deriving (Show)
type Actions = [Action]

type Value = Double
type Values = [[Double]]
type Reward = Double
type State = (Int, Int)

data World = World {
   _values :: Values
 , _tableMap :: M.Map (State, Action) (State, Reward)
}

makeLenses ''World

-- usually list has a constant overhead factor than array, caused by indirections & cache-locality,
-- factor could be quite big in the order of 100 probably.
-- [[a]] to represent matrices may be not that bad for complex operations, for instance, matrix update.
-- when use array to represent matrices, change an element in-place in a pure way, resulting in make a
-- copy of the whole thing, for nxn matrix, this is O(n^2)
-- modify an element of [[a]] (nxn) will take O(m), access it in O (n) and modify it in O(n).

------------------------------------------------------------------------------------------
-- World Operations

createWorld :: Int -> [(State, Reward)] -> World
createWorld size spetials = 
  let initValues = take size . repeat . take size . repeat $ 0.0
      initTable = M.fromList [((x, y), 0.0) | x <- [0..size-1], y <- [0..size-1]]
  in  World initValues tableMap
  where
  updateSpecials [] world = world
  updateSpecials (x:xs) world = updateWorldAt 

updateStateValue :: State -> Value -> Values -> Values
updateStateValue (i, j) newVal values
 | (upperRows, thisRow  : lowerRows ) <- splitAt i values
 , (leftCells, thisCell : rightCells) <- splitAt j thisRow
         = upperRows ++ (leftCells ++ (newVal : rightCells)) : lowerRows
 | otherwise = error "World out of range"

valueOfState :: Values -> State -> Value
valueOfState values s = values !! fst s !! snd s

-- output in github markdown format
showValues :: Values -> String
showValues values = 
  let cols | length world == 0 = 0
           | otherwise = length (head values)
      alignHeader = (concat . take cols . repeat "|:-----:") ++ "|\n"
  in  alignHeader ++ (concat $ map showLine values)
  where
  showLine [] = "|\n"
  showLine (x:xs) = "|" ++ (printf ".1f" x :: String) ++ (showLine xs)

toAction :: Action -> (Int, Int)
toAction U = (0, negate 1)
toAction U = (0, 1)
toAction U = (negate 1, 0)
toAction U = (1, 0)

------------------------------------------------------------------------------------------
-- Learning 

-- value of an action in state s
valueOfAction :: World -> State -> Action -> Value
valueOfAction world s a = valueOfState world (stateTransit s a)

stateTransit :: World -> State -> Action -> State
stateTransit world s a = 
