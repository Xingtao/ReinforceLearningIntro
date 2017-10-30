{-# LANGUAGE ExtendedDefaultRules, ScopedTypeVariables #-}

module TestChapter2Bandit where

import Data.Configurator
import Data.Random
import Data.Random.Distribution
import Data.Random.Source.IO

import Graphics.Matplotlib

import Utils
import Chapter2Bandit

{-

A binding with options, such as that of plot, looks like:

  readData (x, y)
  % mp # "p = plot.plot(data[" # a # "], data[" # b # "]" ## ")"
  % mp # "plot.xlabel(" # str label # ")"

Where important functions are:

readData
Load the given data into the python data array by serializing it to JSON.

%
Sequence two plots

mp
Create an empty plot

#
Append python code to the last command in a plot

##
Just like # but also adds in a placeholder for an options list
You can call this plot with

plot [1,2,3,4,5,6] [1,3,2,5,2] @@ [o1 "go-", o2 "linewidth" 2]
where @@ applies an options list replacing the last ##

o1
A single positional option. 
The value is rendered into python as the appropriate datatype. 
Strings become python strings, bools become bools, etc. 
If you want to insert code verbatim into an option use lit. 
If you want to have a raw string with no escapes use raw.

o2
A keyword option. The key is awlays a string, the value is treated the same way that the option in o1 is treated.
Right now there's no easy way to bind to an option other than the last one unless you want to pass options in as parameters.

The generated Python code should follow some invariants. 
It must maintain the current figure in "fig", all available axes in "axes", and the current axis in "ax". 
Plotting commands should use the current axis, never the plot itself; 
the two APIs are almost identical. 
When creating low-level bindings one must remember to call "plot.sci" to set the current image when plotting a graph. 
The current spine of the axes that's being manipulated is in "spine". The current quiver is in "q"
-}

--drawFigure2_1 :: IO Matplotlib
--drawFigure2_1 kArms = do
--  -- get kArms 'mean value'
--  take kArms randomElement [-1.0..1.0]
--  let normals
--  % mp # "p = plot.plot(data[" # a # "], data[" # b # "]" ## ")"
--  % mp # "plot.xlabel(" # str label # ")"
 
--violinplot (take 3 $ chunksOf 100 $ map (* 2) $ normals) @@ [o2 "showmeans" True, o2 "showmedians" True, o2 "vert" False]
-- 
--mviolinplot = subplots @@ [o2 "ncols" 2, o2 "sharey" True]
--  % setSubplot "0"
--  % violinplot (take 3 $ chunksOf 100 $ map (* 2) $ normals)
--  % setSubplot "1"
--  % violinplot (take 3 $ chunksOf 100 $ map (* 2) $ normals) @@ [o2 "showmeans" True, o2 "showmedians" True, o2 "vert" False]
-- 

testChapter2 :: IO ()
testChapter2 = do
  print "Bandit Experiment Starting, will take several minutes "
  -- readConfigureFile     
  -- code theMat >>= print
  let testN = normal 0.0 1.0
  generateRandomList 100 testN >>= print
