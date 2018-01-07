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
import           Numeric.LinearAlgebra (Vector, Matrix)
import qualified Numeric.LinearAlgebra as LA
import           System.Console.AsciiProgress(Options(..), Stats(..),
                                              displayConsoleRegions, complete,
                                              getProgressStats, def, newProgressBar, tickN)
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
  (width::Int) <- require config "windyGridWorld.worldWidth"
  (height::Int) <- require config "windyGridWorld.worldHeight"
  (epsilon::Double) <- require config "windyGridWorld.epsilon"
  (reward::Double) <- require config "windyGridWorld.reward"  
  (stepSize::Double) <- require config "windyGridWorld.stepSize"
  (startPos::[Int]) <- require config "windyGridWorld.startPos"
  (finishPos::[Int]) <- require config "windyGridWorld.finishPos"
  (windyColumns::[Int]) <- require config "windyGridWorld.windyColumns"
  
  let specials = zipWith3 (\ src dst r -> ((src!!0, src!!1), (dst!!0, dst!!1), r))
                                          specialPositions specialTransitPos specialRewards
  -- do experiments
  mapM_ (goOnePolicyTest size discountGamma learningAccuracy specials) policies
  where
  goOnePolicyTest size discountGamma learningAccuracy specials aPolicy = displayConsoleRegions $ do
    print ("Will do experiment with policy " ++ aPolicy)
    putStrLn ("Special positions: " ++ show specials)
    let thePolicy = read aPolicy
        world = createWorld size thePolicy discountGamma specials
    pg <- newProgressBar def { pgWidth = 80
                             , pgTotal = 100
                             , pgOnCompletion = Just "Done :percent after :elapsed seconds"
                             }
    loop pg world 1
    print "finish chapter3 experiments"
    where
    loop pg world count = do
      let (diff, w') = runState step world
      when (count `mod` 10 == 0)
           (do
            print ("Iteration " ++ show count)
            putStr $ showStateValues (_maxSize w') (_stateValues w')
           )
      case diff < learningAccuracy of
         False -> do
           stat <- getProgressStats pg
           let ticked = stCompleted stat
               percent = floor (100.0 / (diff / learningAccuracy))
           when (ticked < percent) (tickN pg (fromInteger $ percent - ticked))
           loop pg w' (count + 1)
         True -> complete pg >> (putStr $ showStateValues (_maxSize w') (_stateValues w'))
      
