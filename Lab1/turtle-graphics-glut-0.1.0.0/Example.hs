module Main where

import TurtleGraphics
import Turtle
import TurtleExtras

t = (Turtle (300,300) 0 True (0,255,255) (-1) False "" 0 (-1))

main = runGraphical coolExample t

coolExample = 
    ((<|>) 
        (
            ((<|>) (quadProgram(spiral 0 91) 200 )  
                    ((<|>) ((lifespan 50) >*> (spiral 0 91))
                           ((color (255,255,0)) >*> (star 300))
                    )
            )
        )
        ((<|>)((right 45) >*> 
            (quadProgram((color (255,0,0)) 
                          >*> (limited 40 (((spiralForever 0 91))))) 150)) 
            (((<|>) ((die) >*> (forward 200))
                     border))
        )
    )  

coolExample2 = ((<|>) ((forward 100) >*> (lifespan 50) >*> (spiral 0 20)) (spiral 0 91)) >*> (forward 300)
coolExample3 = ((<|>) (((<|>) ((right 45) >*> (forward 100)) 
                               ((left 45) >*> (forward 100)) )) 
                               (((<|>) ((right 90) >*> (forward 100)) 
                                       ((left 90) >*> (forward 100)) )) 
               ) >*> (forward 200)


    
spiral :: Double -> Double -> Program
spiral size angle | size > 100 = (Op Idle)
                  | otherwise  = 
                   ( (forward size) >*> (right angle))
                    >*> spiral (size + 2) angle 

       
spiralForever :: Double -> Double -> Program
spiralForever size angle = 
         ((forward size) >*> (right angle)) 
         >*> spiralForever (size + 2) angle 
         
spiralFiniteToForever :: Double -> Double -> Program
spiralFiniteToForever size angle | size > 100 = spiralForever size angle
                         | otherwise  = 
                         ((forward size) >*> (right angle)) 
                         >*> spiralFiniteToForever (size + 2) angle 
            
            
            

            
            
            
            