{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE BangPatterns         #-}

module TestChapter5MC (
       testChapter5
      )where

import           Control.Concurrent 
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.State

import           Data.Configurator
import           Data.Configurator.Types

import           Data.Maybe(fromJust)
import qualified Data.Map.Strict as M

import           Graphics.Matplotlib hiding (def)
import           System.Console.AsciiProgress(Options(..), Stats(..),
                                              displayConsoleRegions, complete,
                                              getProgressStats, def, newProgressBar, tickN)
import           Text.Printf
-- project
import           Utils
import           Chapter5MC

testChapter5 :: FilePath -> IO ()
testChapter5 configPath = do
  print "Chapter 5 Experiment Starting "
  (config, _) <- autoReload autoConfig [Required configPath]
  (bBlackjackTest::Bool) <- require config "blackjack.bTest"
  (bRacetrackTest::Bool) <- require config "racetrack.bTest"
  when bBlackjackTest (doBlackjackTest config)
  when bRacetrackTest (doRacetrackTest config)

------------------------------------------------------------------------------------------
-- Blackjack Test
doBlackjackTest :: Config -> IO () 
doBlackjackTest config = do
  (episodes::Int) <- require config "blackjack.episodes"
  (ss::Double) <- require config "blackjack.stepSize"
  let !blackjack = mkBlackjack ss                               
  -- do experiments
  displayConsoleRegions $ do
    putStrLn "Will do blackjack experiment "
    pg <- newProgressBar def { pgWidth = 80
                             , pgTotal = 100
                             , pgOnCompletion = Just "Done :percent after :elapsed seconds"
                             }
    blackjack' <- loop pg 1 episodes blackjack
    putStrLn "blackjack experiment finish. Final Results: "
    putStrLn $ show blackjack'
    drawBlackjack episodes blackjack' 
  threadDelay 100000
  where
  loop pg count totalEpisode blackjack 
    | count >= totalEpisode = complete pg >> pure blackjack
    | otherwise = do
      blackjack' <- fst <$> runStateT (blackjackStep count) blackjack
      stat <- getProgressStats pg
      let ticked = fromInteger $ stCompleted stat
          percent = round $ (fromIntegral count) / (fromIntegral totalEpisode)
          willTick = percent - ticked
      tickN pg willTick
      loop pg (count + 1) totalEpisode blackjack'

-- action-state value is the 
drawBlackjack :: Int -> Blackjack -> IO ()
drawBlackjack totalEpisode blackjack = do
  let (hitMap, stickMap) = M.partitionWithKey (\ (_, a) _ -> a == Hit) (_qValues blackjack)
      (hitMap', stickMap') = (M.mapKeys (\(s,_) -> s) hitMap, M.mapKeys (\(s,_) -> s) stickMap)
      !result = M.mergeWithKey (\ _ (_,v1) (_,v2) -> Just ((v1 > v2) ? (Hit, Stick)))
                               (M.mapMaybe (\ _ -> Just Hit))
                               (M.mapMaybe (\ _ -> Just Stick)) hitMap' stickMap'
      (!aceResult, !noAceResult) = M.partitionWithKey (\ (_, _, bAce) _ -> bAce == True) result
  putStrLn "The Results: "
  putStrLn $ prettyPrint aceResult True
  putStrLn $ prettyPrint noAceResult False

prettyPrint :: M.Map (Int, Int, Bool) BJAct -> Bool -> String
prettyPrint result bUseAce = 
  let useIt = bUseAce ? ("Usable Ace: \n", "No Usable Ace: \n")
      header = "|p/d" ++ (concat $ zipWith (\ a b -> a ++ show b) (repeat "|") [1..10])++"|\n"
      alignHeader = (concat . take 11 $ repeat "|:-----:") ++ "|\n"      
  in  useIt ++ header ++ alignHeader ++ (concat $ map showRows [12..21])
  where
  showRows playerSum =
    let title = "|" ++ show playerSum ++ "|"
        cells = concat $ map (fillCell playerSum) [1..10]
    in  title ++ cells ++ "\n"
  fillCell playerSum dealerCard =
    let actStr = show . fromJust $ M.lookup (playerSum, dealerCard, bUseAce) result
    in  actStr ++ "|"

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
---- Racetrack Test

doRacetrackTest :: Config -> IO () 
doRacetrackTest config = do
  ([width, height]::[Int]) <- require config "racetrack.world"
  (discount::Double) <- require config "racetrack.discount"
  (maxV::Int) <- require config "racetrack.maxVelocity"
  (actFailP::Double) <- require config "racetrack.actFailProb"
  (totalEpisodes::Int) <- require config "racetrack.totalEpisodes"
  let !racetrack = mkRacetrack (width, height) discount actFailP maxV
  print (_world racetrack)  
  -- do experiments
  racetrack' <- loop 0 totalEpisodes racetrack
  putStrLn "racetrack experiment finish. Final Results: "
  drawOneRacetrack racetrack'
  threadDelay 100000
  where
  loop count totalEpisode racetrack 
    | count >= totalEpisode = pure racetrack
    | otherwise = do
      putStrLn ("Episode " ++ show count)
      racetrack' <- fst <$> runStateT racetrackStep racetrack
      loop (count + 1) totalEpisode racetrack'
