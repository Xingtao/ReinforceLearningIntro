module Main where

--import TestChapter2Bandit
import TestChapter3MDP
import TestChapter4DP
import TestChapter5MC
import TestChapter6TD

main :: IO ()
main = do
  -- testChapter2 "configs/Chapter2Bandit.cfg"
  -- testChapter3 "configs/Chapter3MDP.cfg"
  -- testChapter4 "configs/Chapter4DP.cfg"
  -- testChapter5 "configs/Chapter5MC.cfg"
  testChapter6 "configs/Chapter6TD.cfg"
