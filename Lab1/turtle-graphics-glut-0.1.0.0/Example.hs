module Main where

import TurtleGraphics
import Turtle
-- import TurtleExtras

main = runGraphical coolExample2 (Turtle (300,300) 0 True (0,255,255))

coolExample = (spiral 0 91 [])
coolExample2 = times 20 ((>*>)(forward 30 []) (right 30 []) [] ) ++ times 40 ((>*>)(forward (10) []) (right (-20) []) [] )

spiral :: Double -> Double -> Program
spiral size angle prog | size > 100 = idle prog
                         | otherwise  = 
            spiral (size + 2) angle ((>*>)(forward size []) (right angle []) prog )
            
            
-- spiralForever :: Double -> Double -> Program
-- spiralForever size angle turtle = 
         -- spiralForever (size + 2) angle (( >*>)(forward size turtle) right angle)
         
-- spiralFiniteToForever :: Program -> Double -> Double -> Program
-- spiralFiniteToForever turtle size angle | size > 100 = spiralForever turtle size angle
                         -- | otherwise  = 
            -- spiralFiniteToForever (forward turtle size >*> right turtle angle) (size + 2) angle