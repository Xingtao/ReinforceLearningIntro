module Main where

import TestChapter2Bandit
import TestChapter3MDP
import TestChapter4DP

main :: IO ()
main = do
  testChapter2 "configs/Chapter2Bandit.cfg"
  -- testChapter3 "configs/Chapter3MDP.cfg"
  -- testChapter4 "configs/Chapter4DP.cfg"

