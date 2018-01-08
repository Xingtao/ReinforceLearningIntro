{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE BangPatterns         #-}

module TestChapter6TD (
       testChapter6
      )where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.State

import           Data.Configurator
import           Data.Configurator.Types                  
import           Data.Text (Text)
-- project
import           Utils
import           Chapter6TD

testChapter6 :: FilePath -> IO ()
testChapter6 configPath = do
  print "Chapter 6 Experiment Starting "
  (config, _) <- autoReload autoConfig [Required configPath]
  (bWindyGridWorld :: Bool) <- require config "enable.bWindyGridWorld"
  when bWindyGridWorld (doWindyGridWorldTest config)
 
doWindyGridWorldTest :: Config -> IO () 
doWindyGridWorldTest config = do
  (totalEpisodes::Int) <- require config "windyGridWorld.totalEpisodes"
  (wWidth::Int) <- require config "windyGridWorld.worldWidth"
  (wHeight::Int) <- require config "windyGridWorld.worldHeight"
  (dEpsilon::Double) <- require config "windyGridWorld.epsilon"
  (dReward::Double) <- require config "windyGridWorld.reward"  
  (dStepSize::Double) <- require config "windyGridWorld.stepSize"
  (aStartPos::[Int]) <- require config "windyGridWorld.startPos"
  (aFinishPos::[Int]) <- require config "windyGridWorld.finishPos"
  (aWindyCols::[Int]) <- require config "windyGridWorld.windyColumns"
  let windyWorld = createWindyWorld wWidth wHeight dEpsilon dStepSize dReward
                     (aStartPos!!0, aStartPos!!1) (aFinishPos!!0, aFinishPos!!1) aWindyCols
  print "Start Stochastic WindyWorld With King's Move Experiment (Sarsa On-Policy Learning)"
  windyWorld' <- doTraining 0 totalEpisodes windyWorld
  trace <- evalStateT runLearningResult windyWorld'  
  print (length trace) >> print trace
  pure ()
  where
  doTraining :: Int -> Int -> WindyWorld -> IO WindyWorld
  doTraining count totalEpisodes windyWorld = do
    case count >= totalEpisodes of
      True -> print "learning finish" >> pure windyWorld
      False -> do
        print $ "Episode " ++ (show count)
        execStateT step windyWorld >>= doTraining (count+1) totalEpisodes

 
