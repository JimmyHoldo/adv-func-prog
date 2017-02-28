module TurtleExtras (
    goLeft
    , goRight
    , doubleProgram
    , quadProgram
    , square
    , upperLeftCorner
    , lowerRightCorner
    , border
    , star
) where

import Turtle

-- | Turn left 90 deg and move forward with given distance
goLeft dist =
  penup >*> (left 90) >*> (forward dist) >*> pendown

-- | Turn right 90 deg and move forward with given distance
goRight dist =
  penup >*> (right 90) >*> (forward dist) >*> pendown

-- | Runs the program twice, separated by goLeft and goRight
doubleProgram prog dist =
  ((<|>) ((goLeft dist)>*> (prog))  ((goRight dist) >*> (prog)))
  
-- | Runs the program doubleProgram twice, separated by goLeft and goRight
quadProgram prog dist =
  ((<|>) ((goLeft dist)>*> (doubleProgram prog dist))  
         ((goRight dist) >*> (doubleProgram prog dist)))
 
-- | Move to upper left corner from position (0,0) 
upperLeftCorner = 
    (goLeft 295) >*> (goLeft 295)>*> (goLeft 0)>*> (goLeft 0)

-- | Move to lower right corner from position (0,0) 
lowerRightCorner = 
    (goRight 295) >*> (goLeft 295)>*> (goRight 0)>*> (goRight 0)

-- | Prints small squares    
square :: Double -> Program
square size   = ((forward size)>*>(right 90)
                  >*>(forward halfSize)>*>(right 90)
                  >*>(forward halfSize)>*>(right 90)
                  >*>(forward halfSize)>*>(left 90)
                  >*>(forward halfSize)>*>(left 90)
                  >*>(forward halfSize)>*>(left 90)
                  >*>(forward halfSize))
          where halfSize = size/2
                                     
-- | Print a square border.                                      
border ::  Program    
border  = ((<|>) (upperLeftCorner 
                   >*> ((<|>) 
                            ((right 45) >*> times 139 (square 6 ))
                            ((left 45) >*> times 139 (square 6 )))) 
                (lowerRightCorner 
                   >*> ((<|>) 
                        ((right 45) >*> times 139 (square 6 ))
                        ((left 45) >*> times 139 (square 6 )))))
                                                  
                                     
-- | Print a star
star :: Double -> Program
star size = (penup) >*> (left 90)
             >*> (forward (size/2)) >*> (pendown) 
             >*> (left 162) >*> (forward (size))
             >*> (left 144) >*> (forward (size))
             >*> (left 144) >*> (forward (size))
             >*> (left 144) >*> (forward (size))
             >*> (left 144) >*> (forward (size))
             >*> (left 144) >*> (forward (size))
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     