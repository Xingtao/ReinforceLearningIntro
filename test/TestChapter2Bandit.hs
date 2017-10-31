{-# LANGUAGE ExtendedDefaultRules, ScopedTypeVariables #-}

module TestChapter2Bandit where

import Control.Concurrent
import Data.Configurator
import Data.Random
import Data.Random.Distribution
import Data.Random.Distribution.Uniform
import Data.Random.Source.IO

import Graphics.Matplotlib

import Utils
import Chapter2Bandit

drawFigure2_1 :: Int -> IO ()
drawFigure2_1 kArms = do
  -- get kArms 'mean value'
  kArmsMeans <- generateRandomList kArms (doubleUniform (-1.0) 1.0)
  let rvars = map (flip normal 1.0) kArmsMeans
  kArmDatas <- mapM (generateRandomList 200) rvars
  let figure2_1 = mp % (violinplot kArmDatas @@ [o2 "showmeans" True, o2 "showmedians" True])
                     % xlabel "Action"
                     % ylabel "Reward"
                     % title "Figure 2-1"
  -- code figure2_1 >>= print
  onscreen figure2_1
  pure ()  


testChapter2 :: IO ()
testChapter2 = do
  print "Bandit Experiment Starting, will take several minutes "
  -- readConfigureFile  
  forkIO $ drawFigure2_1 10
  threadDelay 1000000 >> pure ()
