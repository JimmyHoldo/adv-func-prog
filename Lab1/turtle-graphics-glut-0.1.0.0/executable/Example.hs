module Example where

--import TurtleGraphics
import Turtle
-- import TurtleExtras

--main = runGraphical coolExample

coolExample = spiral 0 10 startTurtle

spiral :: Double -> Double -> Program
spiral size angle turtle | size > 100 = idle turtle
                         | otherwise  = 
            spiral (size + 2) angle (( >*>)(forward size turtle) right angle)
            
            
-- spiralForever :: Program -> Double -> Double -> Program
-- spiralForever turtle size angle = 
         -- spiralForever (forward turtle size >*> right turtle angle) (size + 2) angle
         
-- spiralFiniteToForever :: Program -> Double -> Double -> Program
-- spiralFiniteToForever turtle size angle | size > 100 = spiralForever turtle size angle
                         -- | otherwise  = 
            -- spiralFiniteToForever (forward turtle size >*> right turtle angle) (size + 2) angle