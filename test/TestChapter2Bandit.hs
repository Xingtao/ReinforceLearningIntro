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
import Data.Time
import Graphics.Matplotlib

import Numeric.LinearAlgebra (Vector, Matrix)
import qualified Numeric.LinearAlgebra as LA

import Utils
import Chapter2Bandit


------------------------------------------------------------------------------------------
testChapter2 :: FilePath -> IO ()
testChapter2 configPath = do
  print "Bandit Experiment Starting, will take several minutes "
  
  (config, _) <- autoReload autoConfig [Required configPath]
  (karm :: Int) <- require config "kArms"
  -- 
  (bGreedy :: Bool) <- require config "enable.bGreedyExperiment"
  (bUCB :: Bool) <- require config "enable.bUCBExperiment"
  (bGradient :: Bool) <- require config "enable.bGradientExperiment"
  (bCompareTogether :: Bool) <- require config "enable.bCompareTogether"
  (bFigure2_6 :: Bool) <- require config "enable.bFigure2_6"

  -- do the experiments in order   
  -- EpsilonGreedy Experiment
  (greedyRewardsCurves, greedyActionCurves) <- if bGreedy then doEpsilonGreedyTests config
                                                          else pure (mp, mp)  
  -- UCB Experiment
  (ucbRewardsCurves, ucbActionCurves) <- if bUCB then doUCBTests config else pure (mp, mp)
                                                          
  -- Gradient Experiment
  (gradientRewardsCurves, gradientActionCurves) <- if bGradient then doGradientTests config
                                                      else pure (mp, mp)
  
  when bCompareTogether (drawExperimentsCurves config 
                              (greedyRewardsCurves % ucbRewardsCurves % gradientRewardsCurves,
                               greedyActionCurves % ucbActionCurves % gradientActionCurves))
  when bFigure2_6 (pure ())
  pure ()
  
------------------------------------------------------------------------------------------
-- | doEpsilonGreedyTest Method: compare different epsilons, different initial values,
--   also non-stationary (const step size, weight recent reward more) and stationary environment. 
doEpsilonGreedyTests :: Config -> IO (Matplotlib, Matplotlib)
doEpsilonGreedyTests config = do
  startTime <- getCurrentTime
  print (show startTime ++ ": Start Epsilon Greedy Experiment: ")
  (karm :: Int) <- require config "kArms"
  (totalStep :: Int) <- require config "totalStep"
  (totalBandits :: Int) <- require config "totalBandits"
  (ges :: [Double]) <- require config "greedy.epsilons"
  (initValues :: [Double]) <- require config "greedy.initialOptimalValues"
  (stepValues :: [Double]) <- require config "greedy.stepSizes"
  -- run experiments
  let arrParams = zip3 ges initValues stepValues
  !results <- mapM (\ (ge, initValue, stepValue) -> do
                      let !greedyRVar = bernoulli ge
                      !ret <- replicateM totalBandits 
                                (oneBanditGo karm totalStep initValue stepValue (EGreedy greedyRVar))
                      pure $ (sum ret) / (fromIntegral totalBandits)
                   )
                  arrParams
  resultTime <- getCurrentTime
  print (show resultTime ++ ": Get Epsilon Greedy results, prepare drawing")
  -- draw it
  (ars, oas) <- drawEpsilonGreedy config results
  finishTime <- getCurrentTime
  print $ (show finishTime) ++ ": Finished Epsilon Greedy Experiments"
  pure (ars, oas)
  
------------------------------------------------------------------------------------------
-- | doUCBTest Method
doUCBTests :: Config -> IO (Matplotlib, Matplotlib)
doUCBTests config = do
  startTime <- getCurrentTime
  print (show startTime ++ ": Start UCB Experiment: ")
  -- read all sample average related params
  (karm :: Int) <- require config "kArms"
  (totalStep :: Int) <- require config "totalStep"
  (totalBandits :: Int) <- require config "totalBandits"
  (sss :: [Double]) <- require config "ucb.stepSizes"
  (eds :: [Double]) <- require config "ucb.exploreDegree"
  -- run experiments
  let arrParams = zip sss eds
  !results <- mapM (\ (stepSize, ed) -> do
                     let policy = UCB ed
                     !ret <- replicateM totalBandits (oneBanditGo karm totalStep 0.0 stepSize policy)
                     pure $ (sum ret) / (fromIntegral totalBandits)
                  ) arrParams
  resultTime <- getCurrentTime
  print (show resultTime ++ ": Get UCB results, prepare drawing")  
  -- draw it
  (ars, oas) <- drawUCB config results  
  finishTime <- getCurrentTime
  print $ (show finishTime) ++ ": Finished UCB Experiments"
  pure (ars, oas)

------------------------------------------------------------------------------------------
-- | doUCBTest Method
doGradientTests :: Config -> IO (Matplotlib, Matplotlib)
doGradientTests config = do
  startTime <- getCurrentTime
  print (show startTime ++ ": Start Gradient Experiment: ")
  -- read all sample average related params
  (karm :: Int) <- require config "kArms"
  (totalStep :: Int) <- require config "totalStep"
  (totalBandits :: Int) <- require config "totalBandits"
  (sss :: [Double]) <- require config "gradient.stepSizes"
  (bBaselines :: [Bool]) <- require config "gradient.baselines"
  -- run experiments
  let arrParams = zip sss bBaselines
  !results <- mapM (\ (stepSize, bBase) -> do
                      let policy = Gradient bBase
                      !ret <- replicateM totalBandits (oneBanditGo karm totalStep 0.0 stepSize policy)
                      pure $ (sum ret) / (fromIntegral totalBandits)
                   ) arrParams
  resultTime <- getCurrentTime
  print (show resultTime ++ ": Get Gradient results, prepare drawing")  
  -- draw it
  (ars, oas) <- drawGradient config results  
  finishTime <- getCurrentTime
  print $ (show finishTime) ++ ": Finished Gradient Experiments"
  pure (ars, oas)

------------------------------------------------------------------------------------------
oneBanditGo :: Int -> Int -> Double -> Double -> Policy -> IO (Vector Double)
oneBanditGo karm totalStep initValue stepSize policy = do
  trueValues <- generateRandomList karm stdNormal
  let initBandit = mkBandit karm totalStep initValue stepSize trueValues policy
  (averageRewards, bandit) <- runStateT (loopSteps totalStep) initBandit
  -- join two result lists together for convinent
  pure (LA.fromList $ averageRewards ++ (_bestTakes bandit))

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
                     % grid True
  code figure2_1 >> onscreen figure2_1
  pure ()

drawEpsilonGreedy :: Config -> [Vector Double] -> IO (Matplotlib, Matplotlib)
drawEpsilonGreedy config results = do
  (totalStep :: Int) <- require config "totalStep"
  (ges :: [Double]) <- require config "greedy.epsilons"
  (initValues :: [Double]) <- require config "greedy.initialOptimalValues"
  
  let rewardCurves = foldl (goPlot 0 totalStep) mp (zip3 ges initValues results)
      bestActions = foldl (goPlot totalStep totalStep) mp (zip3 ges initValues results)
      --subplots @@ [o2 "nrows" 2, o2 "sharey" True]
      -- % setSubplot 0 -- % setSubplot 1
      !figureReward = rewardCurves
                       % title "Epsilon-Greedy Average Reward Comparison"
                       % xlabel "Step"
                       % ylabel "Average Reward"
                       % yticks [0.0, 0.2 .. 1.6]
                       % legend @@ [o2 "fancybox" True, o2 "shadow" True, o2 "loc" "lower right"]
                       % grid True
      !figureAction = bestActions                          
                       % title "Epsilon-Greedy Optimal Action Comparison"
                       % xlabel "Step"
                       % ylabel "Optiomal Actions"
                       % yticks [0.0::Double, 0.2 .. 1.0]
                       % legend @@ [o2 "fancybox" True, o2 "shadow" True, o2 "loc" "lower right"]
                       % grid True
                       % tightLayout
  --
  code figureReward >> code figureAction -- avoid Matplotlib's bug
  onscreen figureReward >> onscreen figureAction  
  threadDelay 500000
  pure (rewardCurves, bestActions)
  where
  goPlot :: Int -> Int -> Matplotlib -> (Double, Double, Vector Double) -> Matplotlib
  goPlot startIdx totalLen acc (greedy, initValue, theData) =
    acc % plot [1..totalLen] (LA.subVector startIdx totalLen theData)
               @@ [o2 "label" ("epsilon="++(show greedy)++", Q="++(show initValue))]

drawUCB :: Config -> [Vector Double] -> IO (Matplotlib, Matplotlib)
drawUCB config results = do
  (totalStep :: Int) <- require config "totalStep"
  (sss :: [Double]) <- require config "ucb.stepSizes"    
  let rewardCurves = foldl (goPlot 0 totalStep) mp (zip sss results)
      bestActions = foldl (goPlot totalStep totalStep) mp (zip sss results)
      !figureReward = rewardCurves
                       % title "UCB Average Reward Comparison"
                       % xlabel "Step"
                       % ylabel "Average Reward"
                       % yticks [0.0, 0.2 .. 1.6]
                       % legend @@ [o2 "fancybox" True, o2 "shadow" True, o2 "loc" "lower right"]
                       % grid True
      !figureAction = bestActions                          
                       % title "UCB Optimal Action Comparison"
                       % xlabel "Step"
                       % ylabel "Optiomal Actions"
                       % yticks [0.0::Double, 0.2 .. 1.0]
                       % legend @@ [o2 "fancybox" True, o2 "shadow" True, o2 "loc" "lower right"]
                       % grid True
                       % tightLayout
  --
  code figureReward >> code figureAction -- avoid Matplotlib's bug
  onscreen figureReward >> onscreen figureAction  
  threadDelay 500000
  pure (rewardCurves, bestActions)
  where
  goPlot :: Int -> Int -> Matplotlib -> (Double, Vector Double) -> Matplotlib
  goPlot startIdx totalLen acc (ucbStepSize, theData) =
    acc % plot [1..totalLen] (LA.subVector startIdx totalLen theData)
               @@ [o2 "label" ("ucb, c=" ++ (show ucbStepSize))]

drawGradient :: Config -> [Vector Double] -> IO (Matplotlib, Matplotlib)
drawGradient config results = do
  (totalStep :: Int) <- require config "totalStep"
  (bBaselines :: [Bool]) <- require config "ucb.baselines"
  (sss :: [Double]) <- require config "ucb.stepSizes"  
  let rewardCurves = foldl (goPlot 0 totalStep) mp (zip3 sss bBaselines results)
      bestActions = foldl (goPlot totalStep totalStep) mp (zip3 sss bBaselines results)
      !figureReward = rewardCurves
                       % title "Gradient Average Reward Comparison"
                       % xlabel "Step"
                       % ylabel "Average Reward"
                       % yticks [0.0, 0.2 .. 1.6]
                       % legend @@ [o2 "fancybox" True, o2 "shadow" True, o2 "loc" "lower right"]
                       % grid True
      !figureAction = bestActions                          
                       % title "Gradient Optimal Action Comparison"
                       % xlabel "Step"
                       % ylabel "Optiomal Actions"
                       % yticks [0.0::Double, 0.2 .. 1.0]
                       % legend @@ [o2 "fancybox" True, o2 "shadow" True, o2 "loc" "lower right"]
                       % grid True
                       % tightLayout
  --
  code figureReward >> code figureAction -- avoid Matplotlib's bug
  onscreen figureReward >> onscreen figureAction  
  threadDelay 500000
  pure (rewardCurves, bestActions)
  where
  goPlot :: Int -> Int -> Matplotlib -> (Double, Bool, Vector Double) -> Matplotlib
  goPlot startIdx totalLen acc (alpha, bBaseline, theData) =
    acc % plot [1..totalLen] (LA.subVector startIdx totalLen theData)
               @@ [o2 "label" ("alpha=" ++ (show alpha) ++ (bBaseline?(",with", ",without")))]

drawExperimentsCurves :: Config -> (Matplotlib, Matplotlib) -> IO ()
drawExperimentsCurves config (rewards, actions) = do
  let !figureReward = rewards
                       % title "Average Reward Comparison"
                       % xlabel "Step"
                       % ylabel "Average Reward"
                       % yticks [0.0, 0.2 .. 1.6]
                       % legend @@ [o2 "fancybox" True, o2 "shadow" True, o2 "loc" "lower right"]
                       % grid True
      !figureAction = actions                          
                       % title "Optimal Action Comparison"
                       % xlabel "Step"
                       % ylabel "Optiomal Actions"
                       % yticks [0.0::Double, 0.2 .. 1.0]
                       % legend @@ [o2 "fancybox" True, o2 "shadow" True, o2 "loc" "lower right"]
                       % grid True
                       % tightLayout
  --
  code figureReward >> code figureAction -- avoid Matplotlib's bug
  onscreen figureReward >> onscreen figureAction  
  threadDelay 1000000
  
    

