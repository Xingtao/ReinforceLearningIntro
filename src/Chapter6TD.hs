{-# LANGUAGE TemplateHaskell, RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE MultiParamTypeClasses       #-}
{-# LANGUAGE TypeOperators               #-}
{-# LANGUAGE BangPatterns                #-}

module Chapter6TD
    ( WindyWorld(..)
    , createWindyWorld
    , step
    , showLearningResult
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
-- | WindyWorld
--   Sarsa On-Policy Learning

------------------------------------------------------------------------------------------
-- defs
data Action = U | D | L | R | UL | UR | DL | DR 
  deriving (Show, Ord, Eq)
actions = [U, D, L, R, UL, UR, DL, DR]

type State = (Int, Int)
type SAMap = M.Map (State, Action) Double

data WindyWorld = WindyWorld {
  -- params
    _width :: Policy
  , _height :: Double
  , _epsilon :: Double -- epsilon - greedy select
  , _stepSize :: Double -- epsilon - greedy select
  , _reward :: Double
  , _startPos :: [Int]
  , _finishPos :: [Int]
  , _windyColumns :: [Int]
  -- Learnings
  , _qValMap :: SAMap
} deriving (Show)

makeLenses ''WindyWorld

------------------------------------------------------------------------------------------
-- WindyWorld Operations
createWindyWorld :: Int -> Int -> Double -> Double -> Double -> 
                           (Int, Int) -> (Int, Int) -> [Int] -> WindyWorld
createWindyWorld wWidth wHeight dEpsilon dStepSize dReward aStartPos aFinishPos aWindyCols =
  let states = [(x,y) | x <- [0..wWidth-1], y <- [0..wHeight-1]]
      sas = [(s,a) | s <- states, a <- actions]
      qMap = M.fromList $ zip sas (repeat 0.0)
  in  WindyWorld wWidth wHeight dEpsilon dStepSize dReward aStartPos aFinishPos aWindyCols qMap

showLearningResult :: String
showLearningResult  = 

------------------------------------------------------------------------------------------
-- Helpers
isOutOfRange :: (Int, Int) -> StateCoor -> (Int, Int) -> Bool
isOutOfRange (w, h) s@(x, y) (x', y')
  | x + x' < 0 || x + x' >= w || y + y' < 0 || y + y' >= h = True
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
toMove UL = (negate 1, negate 1)
toMove UR = (1, negate 1)
toMove DL = (negate 1, 1)
toMove DR = (1, 1)

------------------------------------------------------------------------------------------
-- Learning 

runResult :: StateT WindyWorld IO [State]
runResult = runOneEpisode False (_startPos ww)

step :: StateT WindyWorld IO [State]
step = runOneEpisode True (_startPos ww)

runOneEpisode :: (Int, Int) -> WindyWorld -> StateT WindyWorld IO WindyWorld
runOneEpisode s ww = do
  case s == (_finishPos ww) of
     True -> pure ww
     False -> do
      bExplore <- liftIO $ headOrTail (_epsilon ww)     
      let a | bExplore = 
            | otherwise = let candidates = zip (repeat curPos) actions 
                          in  snd $ argmax (fromJust . flip M.lookup (_qValMap ww)) candidates
          (fromJust $ M.lookup sa (_qValMap ww))

          qMap = M.adjust (
      !piMap = M.adjust (const a') s' (_piPolicy racetrack)  
      runOneEpisode curPos' ww = do

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
