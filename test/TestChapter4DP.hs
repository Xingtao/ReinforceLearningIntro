{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE BangPatterns         #-}

module TestChapter4DP (
       testChapter4
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
import           Chapter4DP

testChapter4 :: FilePath -> IO ()
testChapter4 configPath = do
  print "Chapter 3 Experiment Starting "
  (config, _) <- autoReload autoConfig [Required configPath]
  (bCarRental :: Bool) <- require config "enable.bCarRental"
  when bCarRental (doCarRentalTest config)
 
doCarRentalTest :: Config -> IO () 
doCarRentalTest config = do
  (discount::Double) <- require config "carRental.discount"
  (maxCars::[Int]) <- require config "carRental.policy"
  (rentalCredit::[Double]) <- require config "carRental.rentalCredit"
  (transferCost::[Double]) <- require config "carRental.rentalCredit"
  (maxTransferCars::[Int]) <- require config "carRental.maxTransferCars"
  (rentalCars::[Int]) <- require config "carRental.rentalCars"
  (returnCars::[Int]) <- require config "carRental.returnCars"
  (freeParkingLimit::[Int]) <- require config "carRental.freeParkingLimit"
  (additionalParkingCost::[Double]) <- require config "carRental.additionalParkingCost"
  (additionalTransferSaving::[Double]) <- require config "carRental.additionalTransferSaving"

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
    print "finish chapter4 experiments"
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
      
