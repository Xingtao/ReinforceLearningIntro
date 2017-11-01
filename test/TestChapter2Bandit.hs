{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ScopedTypeVariables  #-}

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

mkSampleAverageBandits :: Config -> [RVar Double] -> IO [SampleAverageBandit]
mkSampleAverageBandits config srcRVars = do
  k <- require config "kArms"
  steps <- require config "totalStep"  
  -- sample average method specific parameters
  ges <- require config "sampleAverage.greedyEpsilons"
  initValues <- require config "sampleAverage.initialOptimalValues"
  stepValues <- require config "sampleAverage.stepSizes"
  pure $ zipWith3 (mkOneBandit k steps) ges initValues stepValues
  where
  mkOneBandit k steps ge initValue stepValue = 
    SampleAverageBandit k (take k (repeat initValue)) (take k (repeat 0)) ge
                        stepValue 0.0 steps srcRVars (bernoulli ge)

initSrcDataDistribution :: Int -> IO [RVar Double]
initSrcDataDistribution karm = -- pure $ take karm (repeat stdNormal)
  -- true reward
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

drawFigure2_2 :: Int -> [Double]-> [[Double]] -> IO ()
drawFigure2_2 totalStep greedys averageRewards = do
  let curves = foldl goPlot mp (zip greedys averageRewards)
      figure2_2 = mp % curves
                     % xlabel "Step"
                     % ylabel "Optiomal Reward"
                     % title "Figure 2-2: Average Sample Different Paramters Comparison"

  onscreen figure2_2
  pure ()
  where
  goPlot :: Matplotlib -> (Double, [Double]) -> Matplotlib
  goPlot acc (greedy, averageReward) =
    acc % plot [0 .. (totalStep - 1)] averageReward @@ [o2 "label" ("epsilon="++(show $ greedy))]
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

doSampleAverageTest :: Config -> IO ()
doSampleAverageTest config = do
  karm <- require config "kArms"
  (totalStep :: Int) <- require config "totalStep"
  (totalBandits :: Int) <- require config "sampleAverage.totalBandits"
  -- for drawing
  (ges :: [Double]) <- require config "sampleAverage.greedyEpsilons"
  -- average all: TODO here
  (bestTaskList, averageRewardsList) <- replicateM totalBandits (LA.fromLists <$> goOneRun karm config)
  let !averageRewards = LA.toLists $ (sum averageRewardsList) / (fromIntegral totalBandits)
  drawFigure2_2 totalStep ges averageRewards
  pure ()

  where
  goOneRun karm config =
    initSrcDataDistribution karm >>=
      mkSampleAverageBandits config >>= mapM doSampleAverage
