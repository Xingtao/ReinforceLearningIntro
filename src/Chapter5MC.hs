{-# LANGUAGE TemplateHaskell, RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE MultiParamTypeClasses       #-}
{-# LANGUAGE TypeOperators               #-}
{-# LANGUAGE TypeSynonymInstances        #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE BangPatterns                #-}

module Chapter5MC
    ( Blackjack(..)
    , BJAct(..)
    , Racetrack(..)
    , mkBlackjack
    , blackjackStep
    , mkRacetrack
    , racetrackStep
    ) where

import           Control.Monad
import           Control.Monad.ST
import           Control.Monad.IO.Class
import           Control.Monad.Trans.State

import           Control.Lens (makeLenses, over, element, elements, (+~), (&), (.~), (%~))
import           Data.List(take, repeat)
import           Data.Foldable (toList)
import           Data.Maybe

import qualified Data.Map.Strict as M

import           Data.Random
import           Data.Random.Distribution
import           Data.Random.Distribution.Bernoulli (bernoulli)

import           Language.Haskell.TH

-- project
import           Debug.Trace
import           Utils

------------------------------------------------------------------------------------------
-- | Monte Carlo Method Experiment: Blackjack
--   Will implement: MonteCarlo-E-Soft (On-Policy)

data BJAct = Hit | Stick deriving (Show, Ord, Eq)
-- ((sum, dealer's one card, usable ace), act)
type SAPair = ((Int, Int, Bool), BJAct)

-- epsilon-soft, reduce epsilon as episode increase (epsilon <- 1 / episode count)
data Blackjack = Blackjack {
     _stepSize :: Double -- usually < 0, to use (1/(s,a) occur count) as step size (stationary)
    ,_qValues :: M.Map SAPair (Int, Double) -- (state-action occurs count, sa value)
    } deriving (Show)

makeLenses ''Blackjack

mkBlackjack :: Double -> Blackjack
mkBlackjack stepSize =
  let sas = [((s, d, bAce), a) | s <- [12..21], d <- [1..10],
                                 bAce <- [True, False], a <- [Hit, Stick]]
  in  Blackjack stepSize (M.fromList $ zip sas (repeat (0, 0.0)))

--------------------------------------------------------------------------------
blackjackStep :: Int -> StateT Blackjack IO Blackjack
blackjackStep count = do
  blackjack <- get
  dealerCards <- liftIO $ replicateM 21 nextCard -- at most 21 cards
  playerCards <- liftIO $ replicateM 21 nextCard
  -- player first (explode first ...)  
  let (dealerSum, _) = getSumViaFixedPolicy 17 dealerCards
  trajectories <- liftIO $ generatePlayerTrajectory 
                               blackjack count (head dealerCards) (0,0) playerCards
  let (playerSum, _, _) = fst $ last trajectories
      !reward | playerSum < 0 = negate 1.0
              | playerSum > dealerSum = 1.0
              | playerSum == dealerSum = 0.0
              | otherwise = negate 1.0
      !trajectories' = filter (\ ((ps, _, _), _) -> ps >= 12 && ps <= 21) trajectories
      !blackjack' = foldl (updateValues reward) blackjack trajectories'
  put blackjack'
  pure blackjack'
  where
  updateValues reward blackjack sa =
    let oldValMap = _qValues blackjack
        ss = _stepSize blackjack
    in  blackjack {_qValues = M.adjust (go ss) sa oldValMap}
    where
    go ss (c, v) = if ss < 0 then (c+1, v + (1.0 / fromIntegral (c+1)) * (reward -v))
                      else (c+1, v + (ss * (reward -v)))

generatePlayerTrajectory :: Blackjack -> Int -> Int -> (Int, Int) -> [Int] -> IO [SAPair]
generatePlayerTrajectory _ _ _ _ [] = pure []
generatePlayerTrajectory blackjack count dfc (playerSum, aceNum) (x:xs)
  | playerSum < 11 && x == 1 = generatePlayerTrajectory 
                                   blackjack count dfc (playerSum + 11, aceNum+ 1) xs
  | playerSum <= 11 = generatePlayerTrajectory blackjack count dfc (playerSum + x, aceNum) xs
  | playerSum == 21 = pure [((21, dfc, useAce aceNum), Stick)]
  | playerSum > 21 = if (aceNum > 0)
                        then generatePlayerTrajectory 
                                 blackjack count dfc (playerSum - 10, aceNum - 1) (x:xs)
                        else pure [((negate 1, dfc, False), Stick)] -- blow up
  | playerSum < 21 = do
      a <- epsilonGreedyAct blackjack count (playerSum, dfc, useAce aceNum)
      case a of
        Stick -> pure [((playerSum, dfc, useAce aceNum), Stick)]
        Hit -> (((playerSum, dfc, useAce aceNum), Hit) :) <$>
                 generatePlayerTrajectory blackjack count dfc (playerSum + x, aceNum) xs

epsilonGreedyAct :: Blackjack -> Int -> (Int, Int, Bool) -> IO BJAct
epsilonGreedyAct blackjack count s = do
  bExplore <- headOrTail (1.0 / (fromIntegral count)) -- head means explore
  case bExplore of
    True -> headOrTail 0.5 >>= \ bHit -> pure (bHit ? (Hit, Stick)) -- head means hit
    False -> do
      let (_, hitVal) = fromJust $ M.lookup (s, Hit) (_qValues blackjack)
          (_, standVal) = fromJust $ M.lookup (s, Stick) (_qValues blackjack)
      pure ((hitVal > standVal) ? (Hit, Stick))
                       
--------------------------------------------------------------------------------
-- dealer policy: will stick when sum >= 17
getSumViaFixedPolicy :: Int -> [Int] -> (Int, Int)
getSumViaFixedPolicy standSum = foldl go (0, 0)
  where
  go (acc, aceAs11Num) card | acc < 0 = (acc, aceAs11Num) -- already blow up
                            | acc >= standSum = (acc, aceAs11Num) -- it is ok, just stick
                            | card == 1 && acc + 11 <= 21 = (acc + 11, aceAs11Num + 1)
                            | card == 1 && acc + 11 > 21 = (acc + 1, aceAs11Num)
                            | acc + card > 21 && aceAs11Num > 0 = (acc+card-10, aceAs11Num - 1)
                            | acc + card > 21 = (negate 1, aceAs11Num) -- blow up
                            | otherwise = (acc + card, aceAs11Num)

--------------------------------------------------------------------------------
-- helpers
-- draws with replacement, for randomElement return pure RVar(no memory)
nextCard :: IO Int
nextCard = sample (randomElement [1..13]) >>= \ a -> pure (min a 10)

useAce :: Int -> Bool
useAce num = (num > 0) ? (True, False)

-- epsilon greedy, also random select Hit or Stick
headOrTail :: Double -> IO Bool
headOrTail eps = sample $ bernoulli eps

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
-- | Monte Carlo Method Experiment: Racetrack
--   Will implement: Monte Carlo Off-Policy

-- state: (position, horizon velocity, vertical velocity)
-- act:   (horizon velocity change, vertical velocity change)
type RTAct = (Int, Int)
type RTState = ((Int, Int), (Int, Int))
type RTSA = (RTState, RTAct)

data Racetrack = Racetrack {
     _world :: [[Int]] -- 0 startings, -1 boundaries, -2 ends, > 0 is road
    ,_ww :: Int -- world's width
    ,_wh :: Int -- world's height
    ,_gamma :: Double
    ,_actFailProb :: Double -- there are chances action won't take effect
    ,_maxVelocity :: Int -- max velocity for both horizon & vertical velocity
    ,_piPolicy :: M.Map RTState RTAct -- a deterministic policy
    ,_qValueMap :: M.Map RTSA Double
    ,_cValuesMap :: M.Map RTSA Double
    ,_states :: [RTState]   
    ,_acts :: [RTAct]   
    } deriving (Show)

makeLenses ''Racetrack

------------------------------------------------------------------------------------------
-- Init racetrack 
mkRacetrack :: (Int, Int) -> Double -> Double -> Int -> Racetrack
mkRacetrack (w, h) discount actFailP maxV =
  let !world = genRaceWorld w h
      !allStates = genAllStates world w h maxV
      !allActs = [(aHor, aVer) | aHor <- [-1,0,1], aVer <- [-1,0,1]]
      !saPair = [(s,a) | s <- allStates, a <- allActs]
  in  Racetrack world w h discount actFailP maxV  
                (M.fromList (zip allStates . repeat $ head allActs)) -- targetP 
                (M.fromList (zip saPair [0.0])) -- Q(s,a)
                (M.fromList (zip saPair [0.0])) -- C(s,a)
                allStates allActs

-- generate all possible states, actions
-- filter out according to world ? Right now no, won't depend on world's boundary
genAllStates :: [[Int]] -> Int -> Int -> Int -> [RTState]
genAllStates world w h maxV = 
  [((x,y), (hor,ver)) | x <- [0..w-1], y <- [0..h-1], 
                        hor <- [(negate maxV)..maxV], ver <- [(negate maxV)..maxV],
                        world!!y!!x >= 0]
  
------------------------------------------------------------------------------------------
-- step function
racetrackStep :: StateT Racetrack IO Racetrack 
racetrackStep = genBehaviorEpisodeSeq >>= (\ x -> learningOneEpisode (reverse x) 0.0 1.0)

-- generate via random behavior policy
genBehaviorEpisodeSeq :: StateT Racetrack IO [(RTState, RTAct, Double)]
genBehaviorEpisodeSeq = do
  racetrack <- get
  startPos <- liftIO $ randomStartingPos racetrack
  a <- liftIO $ getRandomAct racetrack (0, 0)
  goUntilFinish racetrack startPos a
  where
  goUntilFinish :: Racetrack -> RTState -> RTAct
                             -> StateT Racetrack IO [(RTState, RTAct, Double)]
  goUntilFinish racetrack s a = do  
    s'@((_, y), v) <- liftIO $ nextPosVelocity racetrack s a
    if y == (_wh racetrack) - 1 
       then pure [(s, a, 1.0)] -- finish episode
       else do
         a' <- liftIO $ getRandomAct racetrack v
         ((s, a, 0.0) :) <$> goUntilFinish racetrack s' a'

nextPosVelocity :: Racetrack -> RTState -> RTAct -> IO RTState
nextPosVelocity racetrack ((x,y), (hor,ver)) (aHor, aVer) = do
  let theWorld = _world racetrack
      x' = x + hor + aHor
      y' = y + ver + aVer
      yFinish = _wh racetrack -1
  case x' < 0 || x' >= _ww racetrack || y' < 0 of
    True -> randomStartingPos racetrack
    False -> do
      case y' >= yFinish && (theWorld!!yFinish!!x' == 100) of
        True -> pure ((x', yFinish), (hor + aHor, ver + aVer))
        False ->
          case y' >= yFinish of
            True -> randomStartingPos racetrack
            False ->
              case theWorld!!y'!!x' < 0 of
                True -> randomStartingPos racetrack
                False -> pure ((x', y'), (hor + aHor, ver + aVer)) 
    
learningOneEpisode :: [(RTState, RTAct, Double)] -> Double -> Double -> StateT Racetrack IO Racetrack
learningOneEpisode [] _ _ = get >>= put
learningOneEpisode ((s@(p,v), a, r) : ss) g w = do
  racetrack <- get
  let g' = r + (_gamma racetrack)*g
      c = fromJust $ M.lookup s (_cValuesMap racetrack)
      q = fromJust $ M.lookup (s,a) (_qValuesMap racetrack)
      c' = c + w -- C_n+1 = C_n + W_n+1
      q' = q + w/c'*(g'-q) -- Q_n+1 = Q_n + W_n+1/C_n+1*(G_n - Q_n)
      qKeys = M.keys (_qValuesMap racetrack)
      qS = filter (\ (x, _) -> x == s) qKeys
      
  put racetrack'
  learningOneEpisode ss

    ,_piPolicy :: M.Map RTState RTAct -- a deterministic policy
    ,_qValueMap :: M.Map RTSA Double
    ,_cValuesMap :: M.Map RTSA Double
  where
  updateQ :: M.Map RTSA Double -> Double -> Double -> RTSA -> Double -> M.Map RTSA Double
  
------------------------------------------------------------------------------------------
-- learning helpers
-- get sensable act with actFail probability    
getRandomAct :: Racetrack -> (Int, Int) -> IO RTAct
getRandomAct racetrack (hor, ver) = do
  bFail <- headOrTail (_actFailProb racetrack)
  case bFail of
    True -> pure (0, 0)
    False -> do
      let maxV = _maxVelocity racetrack
          senseActs = filter (\(aHor, aVer) -> (aHor + hor >= negate maxV) &&
                                               (aHor + hor <= maxV) && (aVer + ver <= maxV) &&
                                               (aVer + ver >= negate maxV)
                             ) (_acts racetrack)
      case null senseActs of 
         True -> pure (0, 0)
         False -> do
           idx <- sample (randomElement [0..(length senseActs - 1)])
           pure (senseActs !! idx)

randomStartingPos :: Racetrack -> IO RTState
randomStartingPos racetrack = do
  let theWorld = _world racetrack
  let startings = filter (\ ((x, y), (hor, ver)) ->
                            y == 0 && (theWorld !! y !! x == 0) && hor == 0 && ver == 0 
                         ) (_states racetrack)
  randomIdx <- sample (randomElement [0..(length startings - 1)])
  pure (startings !! randomIdx)

------------------------------------------------------------------------------------------
{- the world: '=' boundary, '.' starting, '|' is end
   it roughly has the following shape
   ======........===========
   ==---------------------==
   ==---------------------==
   ==---------------------==
   ==---------------------==
   ==---===---==---===-=====
   =======----------------==
   =======----------------==
   =======----------------==
   =======----------------==
   =========--------------==
   ===========------------==
   =============----------==
   ===============--------==
   ===============-------===
   ===============--------==
   ============--------=====
   ===========--------======
   ==========--------=======
   ============-------======
   =========------==========
   =========------==========
   =======-----=============
   =======|||||=============
-}

genRaceWorld :: Int -> Int -> [[Int]]
genRaceWorld w h =  
  let tenthWidth = w `div` 10
      fifthHeight = h `div` 5
      unitBarrier = replicate tenthWidth (negate 1)
      unitRoad = replicate tenthWidth 4
      -- 0 startings, -1 boundaries, -2 ends, > 0 is road
      worldStart = over (elements (< (w `div` 4))) (const (negate 1)) .
                   over (elements (> (w `div` 2))) (const (negate 1)) . take w $ [0..]
      worldStart' = map (\ x -> if x > 0 then 0 else x) worldStart
      barrierLine = take w . concat $ repeat (unitBarrier ++ unitRoad)
      -- part1, head tail is boundary
      part1Line = take w (unitBarrier ++ (concat . replicate 8 $ unitRoad)
                                      ++ (repeat $ negate 1))
      part1 = take fifthHeight $ repeat part1Line
      part1' = map (element (w-1) .~ (negate 1)) part1
      part1'' = [worldStart'] ++ part1' ++ [barrierLine]
      part1Len = length part1''
      part2Len = ((h - part1Len) `div` 2) + 1
      -- part2, head boundary increase
      part2 = map (\x -> take w (unitBarrier ++ (replicate x (negate 1)) ++
                                    (concat $ replicate (fifthHeight*2 - x + 1) unitRoad) ++
                                    (replicate w (negate 1))))
                  [0..fifthHeight*2]
      part2' = map (element (w-1) .~ (negate 1)) $ drop (length part2 - part2Len) part2
      -- part3, tail boundary increase
      part3 = reverse part2'
      !theWorld = take (h-1) (part1'' ++ part2' ++ part3)
      !worldFinishLine = map (\x -> (x > 0) ? (100, x)) (last theWorld)
  in  theWorld ++ [worldFinishLine]
