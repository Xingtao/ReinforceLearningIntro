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
  doBlackjackTest config

----------------------------------------------------------------------------------------------------
-- Car Rental Test
doBlackjackTest :: Config -> IO () 
doBlackjackTest config = do
  (episodes::Int) <- require config "blackjack.episodes"
  (startEpsilon::Double) <- require config "blackjack.startEpsilon"
  let !blackjack = mkBlackjack startEpsilon                               
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
          percent = round $ fromIntegral count / fromIntegral totalEpisode
          willTick = percent - ticked
      tickN pg willTick
      let newEpsilon = 1.0 / (fromIntegral (count + 1))
          newEpsilon' = (newEpsilon < (_epsilon blackjack)) ? (newEpsilon, (_epsilon blackjack))
      loop pg (count + 1) totalEpisode (blackjack' {_epsilon = newEpsilon'})

-- action-state value is the 
drawBlackjack :: Int -> Blackjack -> IO ()
drawBlackjack totalEpisode blackjack = do
  let (hitMap, stickMap) = M.partitionWithKey (\ (_, a) _ -> a == Hit) (_qValues blackjack)
      (hitMap', stickMap') = (M.mapKeys (\ (s, _) -> s) hitMap, M.mapKeys (\ (s, _) -> s) stickMap)
      !result = M.mergeWithKey (\ _ v1 v2 -> Just ((v1 > v2) ? (Hit, Stick)))
                               (M.mapMaybe (\ _ -> Just Hit))
                               (M.mapMaybe (\ _ -> Just Stick)) hitMap' stickMap'
      (!aceResult, !noAceResult) = M.partitionWithKey (\ (_, _, bAce) _ -> bAce == True) result
  putStrLn "The Results: "
  print aceResult
  print noAceResult
