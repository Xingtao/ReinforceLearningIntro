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

import           Data.Random
import           Data.Random.Distribution
import           Data.Random.Distribution.Poisson

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
  print "Chapter 4 Experiment Starting "
  (config, _) <- autoReload autoConfig [Required configPath]
  (bCarRental :: Bool) <- require config "enable.bCarRental"
  when bCarRental (doCarRentalTest config)
 
doCarRentalTest :: Config -> IO () 
doCarRentalTest config = do
  (theTheta::Double) <- require config "carRental.theta"
  (discountGamma::Double) <- require config "carRental.discount"
  (maxCars::[Int]) <- require config "carRental.maxCars"
  (rentalCredit::[Double]) <- require config "carRental.rentalCredit"
  (transCost::[Double]) <- require config "carRental.transferCost"
  (maxTransferCars::[Int]) <- require config "carRental.maxTransferCars"
  (freeParkingLimit::[Int]) <- require config "carRental.freeParkingLimit"
  (additionalParkingCost::[Double]) <- require config "carRental.additionalParkingCost"
  (additionalTransferSaving::[Double]) <- require config "carRental.additionalTransferSaving"
  (rentalCars::[Double]) <- require config "carRental.rentalCars"
  (returnCars::[Double]) <- require config "carRental.returnCars"

  let rentCarR = map poisson rentalCars
      returnCarR = map poisson returnCars
      carRental = mkCarRental (length maxCars) theTheta discountGamma maxCars rentalCredit transCost
                              maxTransferCars freeParkingLimit additionalParkingCost
                              additionalTransferSaving rentCarR returnCarR    
  -- do experiments
  goLoop carRental   
  where
  goLoop carRental = displayConsoleRegions $ do
    putStrLn "Will do car rental experiment " >> putStrLn (show carRental)
    pg <- newProgressBar def { pgWidth = 80
                             , pgTotal = 100
                             , pgOnCompletion = Just "Done :percent after :elapsed seconds"
                             }
    carRental' <- loop pg carRental    
    putStrLn "car rental experiment finish"
    where
    loop pg carRental = do
      ((bFinish, percent), carRental') <- runState step carRental
      case bFinish of
         False -> do
           stat <- getProgressStats pg
           let ticked = fromInteger $ stCompleted stat
               willTick = percent - ticked
           if (willTick > 0) then(tickN pg willTick)
              else print ("Finish One Improvement " ++ show percent) 
           loop pg carRental'
         True -> complete pg >> pure carRental'
