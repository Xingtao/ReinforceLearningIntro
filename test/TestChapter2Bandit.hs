{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE BangPatterns         #-}

module TestChapter2Bandit (
       testChapter2
      )where

import Control.Monad
import Control.Concurrent
import Control.Parallel
import Control.Parallel.Strategies

import Data.Configurator
import Data.Configurator.Types

import Data.Random
import Data.Random.Distribution
import Data.Random.Distribution.Uniform
import Data.Random.Distribution.Bernoulli
import Data.Random.Source.IO

import Data.Text (Text)
import Graphics.Matplotlib

import Numeric.LinearAlgebra (Vector, Matrix)
import qualified Numeric.LinearAlgebra as LA

import Utils
import Chapter2Bandit

------------------------------------------------------------------------------------------
-- initialize helpers
initSrcDataDistribution :: Int -> IO [RVar Double]
initSrcDataDistribution karm = 
  -- pure $ take karm (repeat stdNormal)
  map (flip normal 1.0) <$> generateRandomList karm stdNormal
  -- map (flip normal 1.0) <$> generateRandomList karm (uniform (-1.0) 1.0)

------------------------------------------------------------------------------------------
-- drawings

drawFigure2_1 :: Int -> IO ()
drawFigure2_1 karm = do
  kArmRVars <- initSrcDataDistribution karm
  kArmDatas <- mapM (generateRandomList 200) kArmRVars
  let figure2_1 = mp % (violinplot kArmDatas @@ [o2 "showmeans" True, o2 "showmedians" True])
                     % xlabel "Action"
                     % ylabel "Reward"
                     % title "Figure 2-1"
  onscreen figure2_1
  pure ()

drawFigure2_2 :: Int -> [Double] -> [[Double]] -> [[Double]] -> IO ()
drawFigure2_2 totalStep greedys averageRewards bestActions = do
  let curves = foldl goPlot mp (zip greedys averageRewards)
      bestPercentages = foldl goPlotPercent mp (zip greedys bestActions)
      figure2_2 = mp % subplots
                     % curves
                     % xlabel "Step"
                     % ylabel "Optiomal Reward"
                     % title "Figure 2-2: Average Sample Different Paramters Comparison"
                     % subplots
                     % bestPercentages
                     % xlabel "Step"
                     % ylabel "Best Actions"

  onscreen figure2_2
  pure ()
  where
  goPlot :: Matplotlib -> (Double, [Double]) -> Matplotlib
  goPlot acc (greedy, averageReward) =
    acc % plot [0 .. (totalStep - 1)] averageReward @@ [o2 "label" ("epsilon="++(show $ greedy))]
        % legend @@ [o2 "fancybox" True, o2 "shadow" True, o2 "loc" "lower right"]
  goPlotPercent :: Matplotlib -> (Double, [Double]) -> Matplotlib
  goPlotPercent acc (greedy, bestAction) =
    acc % plot [0..length bestAction-1] bestAction @@ [o2 "label" ("epsilon="++(show $ greedy))]
        % legend @@ [o2 "fancybox" True, o2 "shadow" True, o2 "loc" "lower right"]
  
------------------------------------------------------------------------------------------

testChapter2 :: FilePath -> IO ()
testChapter2 configPath = do
  print "Bandit Experiment Starting, will take several minutes "
  -- readConfigureFile
  (config, _) <- autoReload autoConfig [Required configPath]
  karm <- require config "kArms"
  drawFigure2_1 karm
  -- sample average experiment
  doSampleAverageTest config
  threadDelay 2000000 >> pure ()


-- | Sample Average Method: compare different epsilons, different initial values,
--   also non-stationary (const step size, weight recent reward more) and stationary environment. 
doSampleAverageTest :: Config -> IO ()
doSampleAverageTest config = do
  -- read all sample average related params
  karm <- require config "kArms"
  (totalStep :: Int) <- require config "totalStep"
  (totalBandits :: Int) <- require config "sampleAverage.totalBandits"
  (ges :: [Double]) <- require config "sampleAverage.greedyEpsilons"
  (initValues :: [Double]) <- require config "sampleAverage.initialOptimalValues"
  (stepValues :: [Double]) <- require config "sampleAverage.stepSizes"
  let arrParams = zip3 ges initValues stepValues
  -- [(Vector Double, Vector Double)]
  results <- mapM (\ (ge, initValue, stepVaue) -> 
                     foldM (goOneRun karm totalStep ge initValue stepVaue)
                           (LA.fromList $ take totalBandits (repeat 0.0),
                            LA.fromList $ take totalBandits (repeat 0.0))
                           [1..totalBandits])
                  arrParams
  print "Get results"
  let (averageRewards, bestActions) =
         foldl (\ (acc1, acc2) (r, a) ->
                 ((LA.toList $ (r / fromIntegral totalBandits)) : acc1,
                  (LA.toList $ (a / fromIntegral totalBandits)) : acc2)
               ) ([], []) results
  -- draw it
  drawFigure2_2 totalStep ges averageRewards bestActions
  threadDelay 2000000 >> pure ()  
  pure ()

  where
  goOneRun :: Int -> Int -> Double -> Double -> Double ->
              (Vector Double, Vector Double) -> Int -> IO (Vector Double, Vector Double)
  goOneRun karm totalStep ge initValue stepValue (accBests, accRewards) _ = do
    (rewards, bests) <- mkSampleAverageBandit karm totalStep ge initValue stepValue >>= doSampleAverage
    pure (LA.fromList rewards + accRewards, LA.fromList bests + accBests)

  mkSampleAverageBandit :: Int -> Int -> Double -> Double -> Double -> IO SampleAverageBandit
  mkSampleAverageBandit karm totalStep ge initValue stepValue = do
    srcRVars <- initSrcDataDistribution karm
    pure (SampleAverageBandit karm (take karm (repeat initValue)) (take karm (repeat 0)) ge
                              stepValue 0.0 totalStep [] srcRVars (bernoulli ge))
