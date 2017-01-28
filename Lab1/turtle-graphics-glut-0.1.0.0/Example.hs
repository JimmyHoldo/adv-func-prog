module Main where

import TurtleGraphics
import Turtle
-- import TurtleExtras

main = runGraphical coolExample3 (Turtle (300,300) 0 True (0,255,255) (-1))

coolExample = (spiral 0 91 [])
coolExample2 = times 20 (((>*>)(forward 30 []) (right 30 []) [] ) ++ ((>*>)(forward 30 []) (right 30 []) [] ))
coolExample3 = times 4  (((>*>)((>*>)((>*>) (die []) (penup []) []) (backward (100) []) []) (pendown []) (spiral 0 91 [])) ++ (forward 70 []))

spiral :: Double -> Double -> Program
spiral size angle prog | size > 100 = prog
                         | otherwise  = 
            spiral (size + 2) angle ((>*>)(forward size []) (right angle []) prog )
            
            
spiralForever :: Double -> Double -> Program
spiralForever size angle prog = 
         spiralForever (size + 2) angle ((>*>)(forward size []) (right angle []) prog )
         
-- spiralFiniteToForever :: Program -> Double -> Double -> Program
-- spiralFiniteToForever turtle size angle | size > 100 = spiralForever turtle size angle
                         -- | otherwise  = 
            -- spiralFiniteToForever (forward turtle size >*> right turtle angle) (size + 2) angle