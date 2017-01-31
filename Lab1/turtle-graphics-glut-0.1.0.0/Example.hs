module Main where

import TurtleGraphics
import Turtle
-- import TurtleExtras
t = (Turtle (300,300) 0 True (0,255,255) (-1) False "" 0 (-1))

main = runGraphical coolExample (Turtle (300,300) 0 True (0,255,255) (-1) False "" 0 (-1))

coolExample = (<|>) ((lifespan 50) >*> (spiral 0 91 (Op Idle)))  quadSpiral
coolExample2 = ((lifespan 3) >*> (forward 8) >*> (right 20) >*> (forward 100))
coolExample3 = forward 20
coolExample4 = lifespan 1560 
coolExample5 = (times 20 ((forward 10)>*>(right 90)>*>(forward 20)>*>(right 90) >*>(forward 20)>*>(right 90)))
coolExample6 = ((right 90) >*> (penup) >*> (forward 200) >*> (right 180) >*> (pendown) >*> ((<|>) ((left 20) >*> forward 100) ((right 20) >*> (forward 100))  ))
coolExample7 = (penup) >*> ((<|>) ((forward 200) >*> (pendown) >*> (spiral 0 91 (Op Idle)))  ((backward 200) >*> (pendown) >*> (spiral 0 91 (Op Idle))))
coolExample8 = (goLeft 300) >*> (goLeft 300)>*> (goLeft 0)>*> (goLeft 0) >*> forever (square 0 0 (Op Idle))
coolExample9 = (forward 100)>*>(right 90)>*>(limited 1 ((forward 100)>*>(right 90)>*>(forward 100) ))>*>(forward 100)>*>(forward 100)
coolExample10 = (forward 100)>*>(right 90)>*>(limited 4 ((forward 100)>*>(right 90)>*>(forward 100) ))>*>(forward 100)
--coolExample11 = limited 2 (spiralForever 0 0 (Op Idle))
-- Seq (Seq (Seq (Op (Move 100.0)) (Op (Turn 90.0))) (Limit (Seq (Seq (Op (Move 100.0)) (Op (Turn 90.0))) (Op (Move 100.0))) 2)) (Op (Move 100.0))



spiral :: Double -> Double -> Program -> Program
spiral size angle prog | size > 100 = prog
                       | otherwise  = 
        spiral (size + 2) angle ( prog >*> (forward size) >*> (right angle))

square :: Double -> Double -> Program -> Program
square size angle prog | size > 100 = prog
                         | otherwise  = 
            square (size + 1) angle (prog >*> (forward 5)>*>(right 90)
                                     >*>(forward 2.5)>*>(right 90)
                                     >*>(forward 2.5)>*>(right 90)
                                     >*>(forward 2.5)>*>(left 90)
                                     >*>(forward 2.5)>*>(left 90)
                                     >*>(forward 2.5)>*>(left 90)
                                     >*>(forward 2.5))
            
goLeft dist =
  penup >*> (left 90) >*> (forward dist) >*> pendown

goRight dist =
  penup >*> (right 90) >*> (forward dist) >*> pendown

doubleSpiral =
  ((<|>) ((goLeft 100)>*> (spiral 0 91 (Op Idle)))  ((goRight 100) >*> (spiral 0 91 (Op Idle))))
  

quadSpiral =
  ((<|>) ((goLeft 100)>*> (doubleSpiral))  ((goRight 100) >*> (doubleSpiral)))
            
-- spiralForever :: Double -> Double -> Program -> Program
-- spiralForever size angle prog =
         -- spiralForever (size + 2) angle ( prog >*> (forward size) >*> (right angle))
         
-- spiralFiniteToForever :: Program -> Double -> Double -> Program
-- spiralFiniteToForever turtle size angle | size > 100 = spiralForever turtle size angle
                         -- | otherwise  = 
            -- spiralFiniteToForever (forward turtle size >*> right turtle angle) (size + 2) angle