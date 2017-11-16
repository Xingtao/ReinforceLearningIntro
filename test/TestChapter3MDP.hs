{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE BangPatterns         #-}

module TestChapter3MDP (
       testChapter3
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
import           Chapter3MDP

testChapter3 :: FilePath -> IO ()
testChapter3 configPath = do
  print "Chapter 3 Experiment Starting "
  (config, _) <- autoReload autoConfig [Required configPath]
  (bGridWorld :: Bool) <- require config "enable.bGridWorld"
  when bGridWorld (doGridWorldTest config)
 
doGridWorldTest :: Config -> IO () 
doGridWorldTest config = do
  (size::Int) <- require config "gridWorld.worldSize"
  (policies::[String]) <- require config "gridWorld.policy"
  (discountGamma::Double) <- require config "gridWorld.discount"
  (learningAccuracy::Double) <- require config "gridWorld.learningAccuracy"
  (specialPositions::[[Int]]) <- require config "gridWorld.specialPositions"
  (specialTransitPos::[[Int]]) <- require config "gridWorld.specialTransitPos"
  (specialRewards::[Double]) <- require config "gridWorld.specialRewards"
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
    loop pg world
    print "finish chapter3 experiments"
    where
    loop pg world = do
      let (diff, w') = runState step world
      case diff < learningAccuracy of
         False -> do
           stat <- getProgressStats pg
           let ticked = stCompleted stat
               percent = floor (100.0 / (diff / learningAccuracy))
           when (ticked < percent) (tickN pg (fromInteger $ percent - ticked))
           loop pg w'
         True -> complete pg >> (putStr $ showStateValues (_maxSize w') (_stateValues w'))        
      
