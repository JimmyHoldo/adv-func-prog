module Main where

import TurtleGraphics
import Turtle
-- import TurtleExtras

main = runGraphical coolExample

coolExample = undefined

spiral :: Program -> Double -> Double -> Program
spiral turtle size angle | size > 100 = idle turtle
                         | otherwise  = 
            spiral (forward turtle size >*> right angle) (size + 2) angle