module Main where

import TurtleGraphics
import Turtle
-- import TurtleExtras

main = runGraphical coolExample

coolExample = spiral (Turtle (0.0,0.0) 0 True (0.0, 0.0, 0.0)) 0 0

spiral :: Program -> Double -> Double -> Program
spiral turtle size angle | size > 100 = idle turtle
                         | otherwise  = 
            spiral (forward turtle size >*> right turtle angle) (size + 2) angle
            
            
spiralForever :: Program -> Double -> Double -> Program
spiralForever turtle size angle = 
         spiralForever (forward turtle size >*> right turtle angle) (size + 2) angle
         
spiralFiniteToForever :: Program -> Double -> Double -> Program
spiralFiniteToForever turtle size angle | size > 100 = spiralForever turtle size angle
                         | otherwise  = 
            spiralFiniteToForever (forward turtle size >*> right turtle angle) (size + 2) angle