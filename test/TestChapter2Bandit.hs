{-# LANGUAGE ExtendedDefaultRules, ScopedTypeVariables #-}

module Main where

import Data.Configurator
import Graphics.Matplotlib

degreesRadians a = a * pi / 180.0

main :: IO ()
main = do
  let theMat = contourF (\a b -> sin (degreesRadians a) + cos (degreesRadians b)) (-100) 100 (-200) 200 10
  code theMat >>= print
  onscreen $ theMat
  pure ()
