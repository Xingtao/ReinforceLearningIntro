
{-# LANGUAGE TemplateHaskell, RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE MultiParamTypeClasses       #-}
{-# LANGUAGE TypeOperators               #-}
{-# LANGUAGE BangPatterns                #-}

module Chapter6TD
    ( WindyWorld(..)
    , WWState
    , createWindyWorld
    , step
    , runLearningResult
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
--   Sarsa On-Policy Learning Or Q Off-Policy Learning

------------------------------------------------------------------------------------------
-- defs
data Action = U | D | L | R | UL | UR | DL | DR 
  deriving (Show, Ord, Eq)
actions = [U, D, L, R, UL, UR, DL, DR]

type WWState = (Int, Int)
type SAMap = M.Map (WWState, Action) Double
data LearningMethod = QL | SarsaL
  deriving (Show, Eq, Ord)

data WindyWorld = WindyWorld {
  -- params
    _learnMethod :: LearningMethod
  , _width :: Int
  , _height :: Int
  , _epsilon :: Double -- epsilon - greedy select
  , _stepSize :: Double -- epsilon - greedy select
  , _reward :: Double
  , _startPos :: WWState
  , _finishPos :: WWState
  , _windyColumns :: [Int]
  -- Learnings
  , _qValMap :: SAMap
} deriving (Show)

makeLenses ''WindyWorld

------------------------------------------------------------------------------------------
-- WindyWorld Operations
createWindyWorld :: String -> Int -> Int -> Double -> Double -> Double -> 
                              WWState -> WWState -> [Int] -> WindyWorld
createWindyWorld learningMethod wWidth wHeight dEpsilon
                 dStepSize dReward aStartPos aFinishPos aWindyCols =
  let states = [(x,y) | x <- [0..wWidth-1], y <- [0..wHeight-1]]
      sas = [(s,a) | s <- states, a <- actions]
      qMap = M.fromList $ zip sas (repeat 0.0)
  in  WindyWorld (toLearningMethod learningMethod) wWidth wHeight
                 dEpsilon dStepSize dReward aStartPos aFinishPos aWindyCols qMap

------------------------------------------------------------------------------------------
-- Learning 
   
step :: StateT WindyWorld IO [WWState]
step = do
  ww <- get
  a <- liftIO $ takeAction True ww (_startPos ww)
  runOneEpisode ww True (_startPos ww) a
  
runOneEpisode :: WindyWorld -> Bool -> WWState -> Action -> StateT WindyWorld IO [WWState]
runOneEpisode ww bLearning s a = do
  case s == (_finishPos ww) of
     True -> put ww >> pure []
     False -> do
       !s' <- liftIO $ stateTransit ww s (toMove a)
       !a' <- liftIO $ takeAction (bLearning && (_learnMethod ww == SarsaL)) ww s'
       let !r | s' == (_finishPos ww) = 0
              | otherwise = _reward ww
           !q = fromJust $ M.lookup (s,a) (_qValMap ww)
           !qNext = fromJust $ M.lookup (s',a') (_qValMap ww)
           !q' = q + (_stepSize ww) * (r + 1.0 * qNext - q)
           !qMap' = M.adjust (const q') (s,a) (_qValMap ww)
           !ww' = ww {_qValMap = qMap'}
       (s' :) <$> runOneEpisode ww' bLearning s' a'

-- run
runLearningResult :: StateT WindyWorld IO [WWState]
runLearningResult = do
  ww <- get
  a <- liftIO $ takeAction False ww (_startPos ww)
  runOneEpisode ww False (_startPos ww) a

------------------------------------------------------------------------------------------
---- 
takeAction :: Bool -> WindyWorld -> WWState -> IO Action
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

stateTransit :: WindyWorld -> WWState -> (Int, Int) -> IO WWState
stateTransit ww s a = do
  !windyChange <- randomFromRange ((negate 1),1,1)
  let windyChange' | (_windyColumns ww) !! (fst s) > 0 = windyChange
                   | otherwise = 0
      !windyAct = (0, negate $ windyChange' + ((_windyColumns ww) !! (fst s)))
  pure $ toNextState (_width ww, _height ww) s (addTuple windyAct a)
  
toNextState :: (Int, Int) -> WWState -> (Int, Int) -> WWState
toNextState (w, h) (x, y) (ax', ay') = 
  let x' | x + ax' < 0 = 0
         | x + ax' >= w = w - 1
         | otherwise = x + ax'
      y' | y + ay' < 0 = 0
         | y + ay' >= h = h - 1
         | otherwise = y + ay'
  in  (x', y')

------------------------------------------------------------------------------------------
-- Helpers
toLearningMethod :: String -> LearningMethod
toLearningMethod "Q" = QL
toLearningMethod "Sarsa" = SarsaL
toLearningMethod _ = error "not correct learning method"

toMove :: Action -> (Int, Int)
toMove U = (0, negate 1)
toMove D = (0, 1)
toMove L = (negate 1, 0)
toMove R = (1, 0)
toMove UL = (negate 1, negate 1)
toMove UR = (1, negate 1)
toMove DL = (negate 1, 1)
toMove DR = (1, 1)
