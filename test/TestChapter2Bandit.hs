{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE FlexibleContexts     #-}

module TestChapter2Bandit (
       testChapter2
      )where

import Control.Concurrent
import Data.Configurator
import Data.Configurator.Types

import Data.Random
import Data.Random.Distribution
import Data.Random.Distribution.Uniform
import Data.Random.Distribution.Bernoulli
import Data.Random.Source.IO

import Data.Text (Text)
import Graphics.Matplotlib

import Utils
import Chapter2Bandit

------------------------------------------------------------------------------------------
-- initialize helpers

mkSampleAverageBandits :: Config -> [RVar Double] -> IO [SampleAverageBandit]
mkSampleAverageBandits config srcRVars = do
  k <- require config "kArms"
  loops <- require config "totalRuns"
  -- sample average method specific parameters
  ges <- require config "sampleAverage.greedyEpsilons"
  initValues <- require config "sampleAverage.initialOptimalValues"
  stepValues <- require config "sampleAverage.stepSizes"
  pure $ zipWith3 (mkOneBandit k loops) ges initValues stepValues
  where
  mkOneBandit k loops ge initValue stepValue = 
    SampleAverageBandit k (take k (repeat initValue)) (take k (repeat 0)) ge
                        stepValue 0.0 loops srcRVars (bernoulli ge)

initSrcDataDistribution :: Int -> IO [RVar Double]
initSrcDataDistribution karm =
  map (flip normal 1.0) <$> generateRandomList karm stdNormal

------------------------------------------------------------------------------------------
-- drawings

drawFigure2_1 :: Int -> [RVar Double] -> IO ()
drawFigure2_1 karm kArmRVars = do
  -- get kArms 'mean value'
  -- kArmsMeans <- generateRandomList kArms (doubleUniform (-1.0) 1.0)
  kArmDatas <- mapM (generateRandomList 200) kArmRVars
  let figure2_1 = mp % (violinplot kArmDatas @@ [o2 "showmeans" True, o2 "showmedians" True])
                     % xlabel "Action"
                     % ylabel "Reward"
                     % title "Figure 2-1"
  -- code figure2_1 >>= print
  onscreen figure2_1
  pure ()

drawFigure2_2 :: [SampleAverageBandit] -> [[Double]] -> IO ()
drawFigure2_2 saBandits averageRewards = do
  let curves = foldl goPlot mp (zip saBandits averageRewards)
      figure2_2 = mp % curves
                     % xlabel "Step"
                     % ylabel "Optiomal Reward"
                     % title "Figure 2-2"      
  onscreen figure2_2
  pure ()
  where
  goPlot :: Matplotlib -> (SampleAverageBandit, [Double]) -> Matplotlib
  goPlot acc (saBandit, averageReward) =
    acc % plot [0 .. (_totalRuns saBandit - 1)] averageReward @@ [o2 "label" ("epsilon="++(show $ _greedyEpsilon saBandit))]
        % legend @@ [o2 "fancybox" True, o2 "shadow" True, o2 "loc" "lower right"]

------------------------------------------------------------------------------------------

testChapter2 :: FilePath -> IO ()
testChapter2 configPath = do
  -- readConfigureFile
  (config, _) <- autoReload autoConfig [Required configPath]
  -- using kArmRVars to generate more random datas for repeating runs
  srcDataRVars <- initSrcDataDistribution 10
  
  print "Bandit Experiment Starting, will take several minutes "
  saBandits <- mkSampleAverageBandits config srcDataRVars
  print saBandits

  averageRewards <- mapM doSampleAverage saBandits
  drawFigure2_1 10 srcDataRVars
  drawFigure2_2 saBandits averageRewards
  -- print averageRewards
  
  threadDelay 2000000 >> pure ()
