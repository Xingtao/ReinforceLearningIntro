module Main where

import TestChapter2Bandit
import TestChapter3MDP

main :: IO ()
main = do
  -- testChapter2 "configs/Chapter2Bandit.cfg"
  testChapter3 "configs/Chapter3MDP.cfg"

