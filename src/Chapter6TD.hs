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
-- | Stochastic WindyWorld With King's Move
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
   
step :: StateT WindyWorld IO [State]
step = do
  ww <- get
  a <- liftIO $ takeAction True ww (_startPos ww)
  runOneEpisode ww True (_startPos ww) a
  
runOneEpisode :: WindyWorld -> Bool -> State -> Action -> StateT WindyWorld IO [State]
runOneEpisode ww bLearning s a = do
  case s == (_finishPos ww) of
     True -> put ww >> pure [s]
     False -> do
       s' <- liftIO $ stateTransit ww s a
       a' <- liftIO $ takeAction bLearning ww s'
       let r | s' == (_finishPos ww) = 0
             | otherwise = _reward ww
           q = fromJust $ M.lookup (s,a) (_qValMap ww)
           qNext = fromJust $ M.lookup (s',a') (_qValMap ww)
           q' = q + (_stepSize ww) * (r + 1.0 * qNext - q)
           qMap' = M.adjust (const q') (s,a) (_qValMap ww)
       (s' :) <$> runOneEpisode (ww {_qValMap = qMap'}) bLearning s' a'

-- run
runResult :: StateT WindyWorld IO [State]
runResult = runOneEpisode False (_startPos ww)

------------------------------------------------------------------------------------------
---- 
takeAction :: Bool -> WindyWorld -> State -> IO Action
takeAction bLearning ww s = do
  let candidates = zip (repeat s) actions 
      greedyAction = snd $ argmax (fromJust . flip M.lookup (_qValMap ww)) candidates
  case bLearning  of
    False -> pure greedyAction
    True -> do
      bExplore <- liftIO $ headOrTail (_epsilon ww)
      case bExplore of
        False -> pure greedyAction
        True -> do
          actIdx <- randomFromRange (0,1,(length actions - 1))
          pure (actions !! actIdx)

stateTransit :: WindyWorld -> State -> Action -> IO State
stateTransit ww s a = do
  windyChange <- randomFromRange ((negate 1),0,1)
  let windyAct = (0, windyChange + (_windyColumns !! (fst s)))
  pure $ toNextState (_width ww, _height ww) s (addTuple windyAct a)
  
toNextState :: (Int, Int) -> State -> Action -> State
toNextState (w, h) (x, y) (ax', ay')
  let x' | x + ax' < 0 = 0
         | x + ax' >= w = w - 1
         | otherwise = x + ax'
      y' | y + ay' < 0 = 0
         | y + ay' >= w = w - 1
         | otherwise = y + ay'
  pure (x', y')
