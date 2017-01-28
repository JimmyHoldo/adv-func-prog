-- | proper module documentation here
module Turtle (
  -- * The turtle type(s)
  -- Non-exhaustive list of possible types: Turtle, Program, Action, Operation ...
  Program
  , Turtle (..)
  , Action (..)
  , Operation(..)

  -- * Primitive operations
  , forward
  , right
  , (>*>)
  -- , ...
  , penup
  , pendown
  , color
  , die
   , idle
  -- , limited
  , lifespan
  , times
  , forever
  -- * Derived operations
  , backward
  , left
  -- ...
  , updatePosTurtle
  , updateDegTurtle
  , updateTurtleLife
  , updateTurtlePen
  , updateTurtleCol
  , decreaseTurtleLife
  -- * Run functions
  -- runTextual :: Program -> IO ()
  -- ...

  ) where

-- | Description of your type here...
--
-- | The coordinates of the turtle 
type Position = (Double, Double)

-- | Type for color of the turtle.
type Color = (Double, Double, Double)

-- | Degree of angle
type Angle = Double

--data Action = A(Operation, Turtle, Turtle) 

data Operation = Move Double | Turn Double | Idle | 
                 Col Color | Pen Bool | Die | 
                 Forever | Life Int
            deriving Show

data Turtle = Turtle { 
      position  :: Position
    , direction :: Angle
    , pen       :: Bool
    , col       :: Color
    , life      :: Int
    }     
  deriving Show


--  You can use newtype instead of data if you wish.
-- Should be more than a turtle, perhaps action and operation
data Action = Op Operation | Seq Action Action | F Action | TM [Action] Int
        deriving Show

type Program = [Action] -> [Action]

--type Program = [(Operation, (Turtle, Turtle)) -> [(Operation, (Turtle, Turtle)) ]

-- | Turtle take a step forward.
forward  :: Double -> Program
forward length p = p ++ [Op (Move length)]

            
-- | Turn the turtle right with given angle.
right :: Double -> Program 
right deg p = p ++ [Op (Turn deg)]

            
-- | Perform commands one after another.
(>*>) t1 t2 p = p ++[Seq ((head t1)) ((head t2))]

-- | Penup: stop drawing turtle
penup :: Program
penup p = p ++ [Op (Pen (False))]

-- | Pendown: start drawing turtle
pendown :: Program
pendown p = p ++ [Op (Pen (True))]

-- | Changes the color of the turtle's pen.
color :: Color -> Program
color c p = p ++ [Op (Col (c))]

-- | "Kills" the turtle.
die :: Program
die p = p ++ [Op (Die)]

-- | A program that does nothing.
idle :: Program
idle turtle = turtle ++ [Op (Idle)]
           

-- | Makes the turtle stop what it is doing after a specified period of time.
limited :: Int -> Program
limited = undefined

-- | Kills the turtle after a specified period of time.
lifespan :: Int -> Program
lifespan n p =[Op (Life n)] ++ p

-- | Turtle take a step back.
backward  :: Double -> Program
backward length turtle = forward (-length) turtle

-- | Turn the turtle left with given angle.
left :: Double -> Program 
left angle turtle = right (-angle) turtle

-- | Repeats a turtle program a certain number of times.
times :: Int -> Program
times n p = [(TM (p) n)]

-- | Repeats a program forever.
forever :: [Action] -> [Action] -> [Action]
forever turtle prog = prog ++ [(F (head(turtle)))]

-- | Update a turtles position.
updatePosTurtle :: Double -> Turtle -> Turtle
updatePosTurtle length t = Turtle (newPos) (direction t) (pen t) (col t) (life t)
                    where
                       (x,y) = position t
                       angle = direction t
                       newPos = (x + length * cos (angle * pi / 180), 
                                 y + length * sin (angle * pi / 180))

-- | Update the angle of the turtle
updateDegTurtle :: Double -> Turtle -> Turtle
updateDegTurtle angle t = Turtle (position t) (newDir) (pen t) (col t) (life t)
                    where
                       newDir = direction t + angle
                       
-- | Update the life of the turtle.               
updateTurtleLife :: Int -> Turtle -> Turtle
updateTurtleLife n t = Turtle (position t) (direction t) (pen t) (col t) (n)
                    

-- | Decrease the life of a turtle.
decreaseTurtleLife :: Int -> Turtle -> Turtle
decreaseTurtleLife n t = Turtle (position t) (direction t) (pen t) (col t) (newLife)
                    where newLife | (life t) < 0 = -1
                                  | (life t) > 0 && ((life t) - n) < 0 = 0
                                  | otherwise = (life t) - n

-- | Update if the the turtle draw or not.                    
updateTurtlePen :: Bool -> Turtle -> Turtle
updateTurtlePen b t = Turtle (position t) (direction t) (b) (col t) (life t)

-- | Update the color of the turtle.
updateTurtleCol :: Color -> Turtle -> Turtle
updateTurtleCol c t = Turtle (position t) (direction t) (pen t) (c) (life t)





