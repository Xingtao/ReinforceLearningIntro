{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE BangPatterns         #-}

module TestChapter2Bandit (
       testChapter2
      )where

import Control.Monad
import Control.Monad.Trans.State
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


drawEpsilonGreedy :: Config -> [Vector Double] -> IO ()
drawEpsilonGreedy config results = do
  (totalStep :: Int) <- require config "totalStep"
  (ges :: [Double]) <- require config "greedy.epsilons"
  (initValues :: [Double]) <- require config "greedy.initialOptimalValues"
  
  let rewardCurves = foldl (goPlot 0 totalStep) mp (zip3 ges initValues results)
      bestActions = foldl (goPlot totalStep totalStep) mp (zip3 ges initValues results)
      !figureGreedy = subplots @@ [o2 "nrows" 2, o2 "sharey" True]
                        % setSubplot 0
                        % rewardCurves
                        % title "Epsilon-Greedy Different Paramters Comparison"
                        % xlabel "Step"
                        % ylabel "Average Reward"
                        % yticks [0.0, 0.2 .. 1.6]
                        % legend @@ [o2 "fancybox" True, o2 "shadow" True, o2 "loc" "lower right"]
                        % grid True
                         
                        % setSubplot 1
                        % bestActions
                        % xlabel "Step"
                        % ylabel "Optiomal Actions"
                        % yticks [0.0::Double, 0.2 .. 1.6]
                        % legend @@ [o2 "fancybox" True, o2 "shadow" True, o2 "loc" "lower right"]
                        % grid True
                        % tightLayout
  --
  code figureGreedy >> pure () -- avoid Matplotlib's bug
  onscreen figureGreedy
  threadDelay 1000000 >> pure ()
  where
  goPlot :: Int -> Int -> Matplotlib -> (Double, Double, Vector Double) -> Matplotlib
  goPlot startIdx totalLen acc (greedy, initValue, theData) =
    acc % plot [1..totalLen] (LA.subVector startIdx totalLen theData)
               @@ [o2 "label" ("epsilon="++(show greedy)++", Q="++(show initValue))]
    
------------------------------------------------------------------------------------------
testChapter2 :: FilePath -> IO ()
testChapter2 configPath = do
  print "Bandit Experiment Starting, will take several minutes "
  
  (config, _) <- autoReload autoConfig [Required configPath]
  (karm :: Int) <- require config "kArms"
  -- (bTests :: [Bool]) <- require config "generate.figure1to6"
  -- zipWith when bTests [drawFigure2_1 karm, ]  
    
  -- EpsilonGreedy Experiment
  greedyResults <- doEpsilonGreedyTests config
  -- UCB Experiment
  -- ucbResults <- doUCBTests config
  pure ()

-- | doEpsilonGreedyTest Method: compare different epsilons, different initial values,
--   also non-stationary (const step size, weight recent reward more) and stationary environment. 
doEpsilonGreedyTests :: Config -> IO [Vector Double]
doEpsilonGreedyTests config = do
  -- read all sample average related params
  karm <- require config "kArms"
  (totalStep :: Int) <- require config "totalStep"
  (totalBandits :: Int) <- require config "totalBandits"
  (ges :: [Double]) <- require config "greedy.epsilons"
  (initValues :: [Double]) <- require config "greedy.initialOptimalValues"
  (stepValues :: [Double]) <- require config "greedy.stepSizes"
  -- run experiments
  let arrParams = zip3 ges initValues stepValues
  results <- mapM (\ (ge, initValue, stepValue) -> do
                     ret <- replicateM totalBandits
                                (goOneBanditEpsilonGreedy karm totalStep ge initValue stepValue)
                     pure $ (sum ret) / (fromIntegral totalBandits)
                  )
                  arrParams
  print "Get results"  
  -- draw it
  drawEpsilonGreedy config results
  
  print "Finished Epsilon Greedy Experiments"
  threadDelay 6000000
  pure results

goOneBanditEpsilonGreedy :: Int -> Int -> Double -> Double -> Double -> IO (Vector Double)
goOneBanditEpsilonGreedy karm totalStep ge initValue stepSize = do
  trueValues <- generateRandomList karm stdNormal
  let greedyRVar = bernoulli ge
      initBandit = mkBandit karm totalStep initValue stepSize trueValues (EGreedy greedyRVar)
  (averageRewards, bandit) <- runStateT (loopSteps totalStep) initBandit
  -- join two result lists together for convinent
  pure (LA.fromList $ averageRewards ++ (_bestTakes bandit))
