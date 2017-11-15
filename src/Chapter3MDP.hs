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

type World = [[Double]]
type Reward = Double
type Value = Double
type State = (Int, Int)

-- usually list has a constant overhead factor than array, caused by indirections & cache-locality,
-- factor could be quite big in the order of 100 probably.
-- [[a]] to represent matrices may be not that bad for complex operations, for instance, matrix update.
-- when use array to represent matrices, change an element in-place in a pure way, resulting in make a
-- copy of the whole thing, for nxn matrix, this is O(n^2)
-- modify an element of [[a]] (nxn) will take O(m), access it in O (n) and modify it in O(n).

------------------------------------------------------------------------------------------
-- World Operations

createWorld :: Int -> [((Int, Int), Double)] -> World
createWorld size spetials =
  updateSpecials specials . take size . repeat . take size $ repeat 0.0
  where
  updateSpecials [] world = world
  updateSpecials (x:xs) world = updateWorldAt 

updateWorldAt :: (Int, Int) -> (Double -> Double) -> World -> World
updateWorldAt (i, j) f mat
 | (upperRows, thisRow : lowerRows ) <- splitAt i mat
 , (leftCells, thisCell: rightCells) <- splitAt j thisRow
         = upperRows ++ (leftCells ++ (f thisCell): rightCells) : lowerRows
 | otherwise = error "World out of range"

-- output in github markdown format
showWorld :: World -> String
showWorld world = 
  let cols | length world == 0 = 0
           | otherwise = length (head world)
      alignHeader = (concat . take cols . repeat "|:-----:") ++ "|\n"
  in  alignHeader ++ (concat $ map showLine world)
  where
  showLine [] = "|\n"
  showLine (x:xs) = "|" ++ (printf ".1f" x :: String) ++ (showLine xs)
-- . | left align | center align  | right align |
-- . | :------------ |:---------------:| -----:|
-- . | col 1         |  text           | $1    |
-- . | col 2 is      | centered        |   $12 |

------------------------------------------------------------------------------------------
-- Learning 

valueOfState :: World -> State -> Value
valueOfState world s = world !! fst s !! snd s

-- value of an action in state s
valueOfAction :: World -> State -> Action -> Value
valueOfAction world s a = valueOfState world (stateTransit s a)

stateTransit :: World -> State -> Action -> State
stateTransit world s a = 
