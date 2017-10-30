{-# LANGUAGE ExtendedDefaultRules, ScopedTypeVariables #-}

module TestChapter2Bandit where

import Data.Configurator
import Graphics.Matplotlib

import Utils
import Chapter2Bandit

--degreesRadians a = a * pi / 180.0
-- 
--main :: IO ()
--main = do
--  let theMat = contourF (\a b -> sin (degreesRadians a) + cos (degreesRadians b)) (-100) 100 (-200) 200 10
--  code theMat >>= print
--  onscreen $ theMat
--  pure ()

-- 1. make random distribution
-- 2. draw violin pic

testChapter2 :: IO ()
testChapter2 = pure ()
