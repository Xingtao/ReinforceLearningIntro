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
-- drawings

drawFigure2_1 :: Int -> IO ()
drawFigure2_1 karm = do
  kArmRVars <- map (flip normal 1.0) <$> generateRandomList karm stdNormal
  kArmDatas <- mapM (generateRandomList 200) kArmRVars
  let figure2_1 = mp % (violinplot kArmDatas @@ [o2 "showmeans" True, o2 "showmedians" True])
                     % xlabel "Action"
                     % ylabel "Reward"
                     % title "Figure 2-1"
  onscreen figure2_1
  pure ()

drawFigure2_2 :: Int -> [Double] -> [Vector Double] -> IO ()
drawFigure2_2 totalStep greedys rbs = do
  let (!rewardCurves, !bestPercentages) = foldl goPlot (mp, mp) (zip greedys rbs)
      !figure2_2 = subplots @@ [o2 "nrows" 2, o2 "sharey" True]
                     % setSubplot 0
                     % title "Figure 2-2: Average Sample Different Paramters Comparison"
                     % xlabel "Step"
                     % ylabel "Optiomal Reward"
                     % rewardCurves
                     % setSubplot 1
                     % xlabel "Step"
                     % ylabel "Best Actions"
                     % bestPercentages
                     % tightLayout
  code figure2_2 >> pure () -- avoid Matplotlib's bug
  onscreen figure2_2
  threadDelay 2000000 >> pure ()
  where
  goPlot :: (Matplotlib, Matplotlib) -> (Double, Vector Double) -> (Matplotlib, Matplotlib)
  goPlot (accReward, accBestAction) (greedy, rb) =
    let !splitsRB = LA.takesV [totalStep, totalStep] rb
    in  (accReward % plot [1..totalStep] (splitsRB!!0) @@ [o2 "label" ("epsilon="++(show greedy))]
                   % legend @@ [o2 "fancybox" True, o2 "shadow" True, o2 "loc" "lower right"],
         accBestAction % plot [1..totalStep] (splitsRB!!1)
                              @@ [o2 "label" ("epsilon="++(show greedy))]
                       % legend @@ [o2 "fancybox" True, o2 "shadow" True, o2 "loc" "lower right"]
        )
  
------------------------------------------------------------------------------------------
testChapter2 :: FilePath -> IO ()
testChapter2 configPath = do
  print "Bandit Experiment Starting, will take several minutes "
  -- readConfigureFile
  (config, _) <- autoReload autoConfig [Required configPath]
  (karm :: Int) <- require config "kArms"
  drawFigure2_1 karm
  -- sample average experiment
  doSampleAverageTest config
  pure ()

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
  -- run experiments
  let arrParams = zip3 ges initValues stepValues
  !results <- mapM (\ (ge, initValue, stepValue) -> do
                     temp <- replicateM totalBandits
                               (mkSampleAverageBandit karm totalStep
                                  ge initValue stepValue >>= doSampleAverage)
                     pure $ (sum temp) / (fromIntegral totalBandits)
                  )
                  arrParams
  print "Get results"  
  -- draw it
  drawFigure2_2 totalStep ges results
  
  print "Finished Average Sample Experiments"
  threadDelay 6000000 >> pure ()
  where
  mkSampleAverageBandit :: Int -> Int -> Double -> Double -> Double -> IO SampleAverageBandit
  mkSampleAverageBandit karm totalStep ge initValue stepValue = do
    trueValues <- generateRandomList karm stdNormal
    let (maxValueIdx, _) = argmaxWithIndex (zip [0..] trueValues)
    pure (SampleAverageBandit karm (take karm (repeat initValue)) maxValueIdx
                              (take karm (repeat 0)) ge stepValue 0.0 totalStep []
                              (map (flip normal 1.0) trueValues) (bernoulli ge))
