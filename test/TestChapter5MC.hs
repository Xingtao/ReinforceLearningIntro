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
import           Data.List.Split (chunksOf)
import           Data.Foldable (toList)

import           Data.Random
import           Data.Random.Distribution
import           Data.Random.Distribution.Poisson

import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Text (Text)

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
  (epsilonGreedy::Double) <- require config "blackjack.epsilon"
  let !blackjack = mkBlackjack epsilonGreedy                               
  -- do experiments
  displayConsoleRegions $ do
    putStrLn "Will do blackjack experiment " >> putStrLn (show blackjack)
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
      loop pg (count + 1) totalEpisode blackjack'

-- action-state value is the 
drawBlackjack :: Int -> Blackjack -> IO ()
drawBlackjack totalEpisode blackjack = pure ()
