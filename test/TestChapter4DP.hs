{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE BangPatterns         #-}

module TestChapter4DP (
       testChapter4
      )where

import           Control.Concurrent 
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.State

import           Data.Configurator
import           Data.Configurator.Types
import           Data.List.Split (chunksOf)
import           Data.Foldable (toList)

import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Text (Text)

import           Data.Random
import           Data.Random.Distribution
import           Data.Random.Distribution.Poisson

import           Graphics.Matplotlib hiding (def)

import           Numeric.LinearAlgebra (Vector, Matrix)
import qualified Numeric.LinearAlgebra as LA
import           System.Console.AsciiProgress(Options(..), Stats(..),
                                              displayConsoleRegions, complete,
                                              getProgressStats, def, newProgressBar, tickN)
import           Text.Printf
-- project
import           Utils
import           Chapter4DP

testChapter4 :: FilePath -> IO ()
testChapter4 configPath = do
  print "Chapter 4 Experiment Starting "
  (config, _) <- autoReload autoConfig [Required configPath]
  (bCarRental :: Bool) <- require config "enable.bCarRental"
  when bCarRental (doCarRentalTest config)
  (bGambler :: Bool) <- require config "enable.bGamblerProblem"
  when bGambler (doGamblerTest config)

----------------------------------------------------------------------------------------------------
-- Car Rental Test
doCarRentalTest :: Config -> IO () 
doCarRentalTest config = do
  (experimentName::String) <- require config "carRental.experiment"
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

  let carRental = mkCarRental (length maxCars) theTheta discountGamma maxCars rentalCredit transCost
                              maxTransferCars freeParkingLimit additionalParkingCost
                              additionalTransferSaving rentalCars returnCars    
  -- do experiments
  carRental' <- goLoop carRental
  when (_locationNum carRental == 2) (drawTwoLocationCarRental carRental' experimentName)
  threadDelay 100000
  where
  goLoop carRental = displayConsoleRegions $ do
    putStrLn "Will do car rental experiment " >> putStrLn (show carRental)
    pg <- newProgressBar def { pgWidth = 80
                             , pgTotal = 100
                             , pgOnCompletion = Just "Done :percent after :elapsed seconds"
                             }
    carRental' <- loop pg carRental
    putStrLn "car rental experiment finish. Final Results: "
    putStrLn $ showCarRentalResult carRental'
    pure carRental'
    where
    loop pg carRental = do
      let (!(bFinish, percent), !carRental') = runState carRentalStep carRental
      case bFinish of
         False -> do
           stat <- getProgressStats pg
           let ticked = fromInteger $ stCompleted stat
               willTick = percent - ticked
           if (willTick > 0) then(tickN pg willTick)
              else print ("Finish One Improvement " ++ show percent) 
           loop pg carRental'
         True -> complete pg >> pure carRental'

drawTwoLocationCarRental :: CarRental -> String -> IO ()
drawTwoLocationCarRental carRental experimentName = do
  putStrLn "Draw Two Location Car Rental Result Graph"
  -- dataX is the second location, whereas dataY is the first location
  let dataX = toList $ fmap (\ (f:s:[]) -> s) (_states carRental) 
      dataY = toList $ fmap (\ (f:s:[]) -> f) (_states carRental)
      (dataZ::[Int]) = toList (Seq.zipWith (\ i saPair -> calcZ . foldl (zipWith (+)) [0, 0] $ Seq.index saPair i)
                                           (_actions carRental) (_possibleActions carRental))
      dataValue = toList $ _stateValues carRental
      maxZ = maximum dataZ
      figure = readData ([dataX], [dataY], [dataZ])
                 % mp # "ax = plot.gca(projection='3d')"
                 % mp # "ax.scatter(np.array(data[" # 0 # "]), np.array(data[" # 1 # "]), np.array(data[" # 2 # "]))"
                 % xlabel "#Cars at second location"
                 % ylabel "#Cars at first location"
                 % zlabel "#Cars to move (action)"
                 % xticks [0, 1..(_maxCars carRental !! 1)]
                 % yticks [0, 1..(_maxCars carRental !! 0)]
                 % zticks [negate maxZ, (negate $ maxZ - 1)..maxZ]
                 % title experimentName
      valueFigure = readData ([dataX], [dataY], [dataValue])
                      % mp # "ax = plot.gca(projection='3d')"
                      % mp # "ax.scatter(np.array(data[" # 0 # "]), np.array(data[" # 1 # "]), np.array(data[" # 2 # "]))"
                      % xlabel "#Cars at second location"
                      % ylabel "#Cars at first location"
                      % zlabel "State Values"
                      % xticks [0, 1..(_maxCars carRental !! 1)]
                      % yticks [0, 1..(_maxCars carRental !! 0)]
                      % title (experimentName ++ " - State Values")
  -- avoid Matplotlib's bug  
  code figure >> code valueFigure >> onscreen figure >> onscreen valueFigure
  threadDelay 100000
  -- also output table format
  let moveResult = showAsTable [0..(_maxCars carRental)!!0]
                               [0..(_maxCars carRental)!!1] (map fromIntegral dataZ) False
      stateResult = showAsTable [0..(_maxCars carRental)!!0]
                                [0..(_maxCars carRental)!!1] dataValue True
  putStrLn moveResult
  putStrLn stateResult
  where
  calcZ :: [Int] -> Int
  calcZ (firstLocation:secondLocation:[]) = 
            (firstLocation > 0) ? (negate firstLocation, secondLocation)
  calcZ _ = error "Not correct action move"

-- output in github markdown format
showAsTable :: [Int] -> [Int] -> [Double] -> Bool -> String
showAsTable col row vals bFloat =
  let colLen = length col
      header = "|First/second| " ++ (concat $ map ((++ "|") . show) col) ++ "\n"
      alignHeader = (concat . take (colLen + 1) $ repeat "|:-----:") ++ "|\n"
      showRows = concat $ map (go colLen vals) row
  in  header ++ alignHeader ++ showRows
  where
  go len vals rowIdx =
    let first = show rowIdx ++ "|"
        rows = take len $ drop (len * rowIdx) vals
        format = bFloat ? ("%7.2f", "%7.0f")
    in  first ++ (concat $ map (\ x -> (printf format x :: String) ++ "|") rows) ++ "|\n"

----------------------------------------------------------------------------------------------------
-- Gambler Problem Test

doGamblerTest :: Config -> IO () 
doGamblerTest config = do
  (prob::Double) <- require config "gambler.headProb"
  (gamblerGoal::Int) <- require config "gambler.goal"
  (theTheta::Double) <- require config "gambler.theta"
  let gambler = mkGambler prob theTheta gamblerGoal
  -- do experiments
  let (_, !gambler') = runState gamblerStep gambler
  drawGamblerGraph gambler'
  threadDelay 100000
  putStrLn $ showGamblerResult gambler'
  pure ()

drawGamblerGraph :: Gambler -> IO ()
drawGamblerGraph gambler = pure ()
