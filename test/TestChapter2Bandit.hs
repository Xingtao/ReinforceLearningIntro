{-# LANGUAGE ExtendedDefaultRules, ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

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

mkSampleAverageBandit :: Config -> [RVar Double] -> IO SampleAverageBandit
mkSampleAverageBandit config srcRVars = do
  k <- require config "kArms"
  ge <- require config "greedyEpsilon"
  initValue <- require config "sampleAverage.initialOptimalValue"
  stepValue <- require config "sampleAverage.stepSize"
  pure $ SampleAverageBandit k (take k (repeat initValue))
                             (take k (repeat 0)) stepValue 0.0 srcRVars (bernoulli ge)

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

drawFigure2_2 :: SampleAverageBandit -> [Double] -> IO ()
drawFigure2_2 saBandit averageRewards = do  
  -- onscreen figure2_2
  pure ()
  

------------------------------------------------------------------------------------------

testChapter2 :: FilePath -> IO ()
testChapter2 configPath = do
  -- readConfigureFile
  (config, _) <- autoReload autoConfig [Required configPath]
  -- using kArmRVars to generate more random datas for repeating runs
  srcDataRVars <- initSrcDataDistribution 10
  
  print "Bandit Experiment Starting, will take several minutes "
  saBandit <- mkSampleAverageBandit config srcDataRVars

  averageRewards <- doSampleAverage 500 saBandit
  drawFigure2_1 10 srcDataRVars
  -- print averageRewards

  drawFigure2_2 saBandit averageRewards

  threadDelay 2000000 >> pure ()
