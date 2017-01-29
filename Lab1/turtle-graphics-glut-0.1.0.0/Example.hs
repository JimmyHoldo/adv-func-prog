module Main where

import TurtleGraphics
import Turtle
-- import TurtleExtras

main = runGraphical coolExample (Turtle (300,300) 0 True (0,255,255) (-1) False)

coolExample = (<|>) ((lifespan 50) >*> (spiral 0 91 (Empty)))  quadSpiral
coolExample2 = ((lifespan 3) >*> (forward 8) >*> (right 20) >*> (forward 100))
coolExample3 = forward 20
coolExample4 = lifespan 1560 
coolExample5 = forever (forward 2)
coolExample6 = ((right 90) >*> (penup) >*> (forward 200) >*> (right 180) >*> (pendown) >*> ((<|>) ((left 20) >*> forward 100) ((right 20) >*> (forward 100))  ))
coolExample7  = (penup) >*> ((<|>) ((forward 200) >*> (pendown) >*> (spiral 0 91 (Empty)))  ((backward 200) >*> (pendown) >*> (spiral 0 91 (Empty))))

spiral :: Double -> Double -> Program -> Program
spiral size angle prog | size > 100 = prog
                         | otherwise  = 
            spiral (size + 2) angle  ( prog>*>(forward size) >*> (right angle))
            
goLeft dist =
  penup >*> (left 90) >*> (forward dist) >*> pendown

goRight dist =
  penup >*> (right 90) >*> (forward dist) >*> pendown

doubleSpiral =
  ((<|>) ((goLeft 100)>*> (spiral 0 91 Empty))  ((goRight 100) >*> (spiral 0 91 Empty)))
  

quadSpiral =
  ((<|>) ((goLeft 100)>*> (doubleSpiral))  ((goRight 100) >*> (doubleSpiral)))
            
-- spiralForever :: Double -> Double -> Program
-- spiralForever size angle prog = 
         -- spiralForever (size + 2) angle prog ++ ((>*>)(forward size []) (right angle []) )
         
-- spiralFiniteToForever :: Program -> Double -> Double -> Program
-- spiralFiniteToForever turtle size angle | size > 100 = spiralForever turtle size angle
                         -- | otherwise  = 
            -- spiralFiniteToForever (forward turtle size >*> right turtle angle) (size + 2) angle