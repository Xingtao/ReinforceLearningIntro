{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE BangPatterns         #-}

module TestChapter3MDP (
       testChapter3
      )where

import           Control.Monad
import           Data.Configurator
import           Data.Configurator.Types                  
import           Data.Text (Text)
import           Numeric.LinearAlgebra (Vector, Matrix)
import qualified Numeric.LinearAlgebra as LA
import           System.Console.AsciiProgress(Options(..), displayConsoleRegions,
                                              isComplete, def, newProgressBar, tick)
-- project
import           Utils
import           Chapter3MDP

main :: IO ()
main = displayConsoleRegions $ do

testChapter3 :: FilePath -> IO ()
testChapter3 configPath = displayConsoleRegions $ do
  print "Chapter 3 Experiment Starting "
  (config, _) <- autoReload autoConfig [Required configPath]
  (bGridWorld :: Bool) <- require config "enable.bGridWorld"
  when bGridWorld doGridWorldTest config
 
doGridWorldTest :: Config -> IO () 
doGridWorldTest config = do
  (size::Int) <- require config "gridWorld.worldSize"
  (policies::[String]) <- require config "gridWorld.policy"
  (discountGamma::Double) <- require config "gridWorld.discount"
  (learningAccuracy::Double) <- require config "gridWorld.learningAccuracy"
  (specialPositions::[[Int]]) <- require config "gridWorld.specialPositions"
  (specialTransitPos::[[Int]]) <- require config "gridWorld.specialTransitPos"
  (specialRewards::[Double]) <- require config "gridWorld.specialRewards"
  let specials = zipWith (\ src dst r -> ((src!!0, src!!1), (dst!!0, dst!!1), r)
                         specialPositions specialTransitPos specialRewards
  -- do experiments
  mapM_ (goOnePolicyTest size discount learningAccuracy specials) policies
  where
  goOnePolicyTest size discountGamma learningAccuracy specials) p = do
    print ("Will do experiment with policy " ++ p ++ " and special positions" ++ show specials)
   


  createWorld :: Int -> Policy -> Double -> [(State, State, Reward)] -> World
  createWorld size p gamma specials = 

     specialPositions  = [[0, 1], [0, 4]]
     specialTransitPos = [[1, 4], [3, 2]]
     specialRewards    = [10.0,   5.0]

  pg <- newProgressBar def { pgWidth = 100
                           , pgOnCompletion = Just "Done :percent after :elapsed seconds"
                           }
  loop pg
  print "finish chapter3 experiments"
  pure ()
  where
  loop pg = do
    b <- isComplete pg
    unless b $ do
        threadDelay $ 200 * 1000
        tick pg
        loop pg  
  
