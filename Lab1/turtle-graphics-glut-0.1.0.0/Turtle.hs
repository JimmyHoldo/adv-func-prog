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
  -- , penup
  -- , pendown
  -- , color
  -- , die
   , idle
  -- , limited
  , lifespan
  , backward
  , left
  , times
  , forever
  -- * Derived operations
  -- ...
  , updatePosTurtle
  , updateDegTurtle
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

data Operation = Start Turtle | Move Double | Turn Double | 
                 Idle | Col | Penup | Pendown | Die | Forever |
                 Times | Life Int
            deriving Show

data Turtle = Turtle { 
      position  :: Position
    , direction :: Angle
    , pen       :: Bool
    , col       :: Color
    }     
  deriving Show


--  You can use newtype instead of data if you wish.
-- Should be more than a turtle, perhaps action and operation
data Action = Op Operation | Seq Action Action | F Action | TM Action Int
        deriving Show

type Program = [Action] -> [Action]

--type Program = [(Operation, (Turtle, Turtle)) -> [(Operation, (Turtle, Turtle)) ]

-- | Turtle take a step forward.
forward  :: Double -> Program
forward length p = p ++ [Op (Move length)]
-- forward  :: Double -> Program
-- forward length turtle = turtle ++ [(Move, (newTurtle, t))]
        -- where 
            -- t = fst . snd $ last turtle
            -- (x,y) = position t
            -- angle = direction t
            -- newPos = (x + length * cos (angle * pi / 180), 
                               -- y + length * sin (angle * pi / 180))
            -- newTurtle = Turtle (newPos) (direction t) (pen t) (col t)
            
-- | Turn the turtle right with given angle.
right :: Double -> Program 
right deg p = p ++ [Op (Turn deg)]
-- right angle turtle = turtle ++ [(Turn, (newTurtle, t))]
        -- where 
            -- t = fst . snd $ last turtle
            -- newDir = direction t + angle
            -- newTurtle = Turtle (position t) (newDir) (pen t) (col t)
            
-- | Perform commands one after another.
(>*>) t1 t2 p = p ++[Seq ((head t1)) ((head t2))]
--prog >*> next = [Seq ((head prog)) ((head next))] 


-- | Penup: stop drawing turtle
-- penup :: Program
-- penup turtle = turtle ++ [(Penup, (newTurtle, t))]
        -- where
           -- t = fst . snd $ last turtle
           -- newTurtle = Turtle (position t) (direction t) (False) (col t)
-- | Pendown: start drawing turtle
-- pendown :: Program
-- pendown turtle = turtle ++ [(Pendown, (newTurtle, t))]
        -- where
           -- t = fst . snd $ last turtle
           -- newTurtle = Turtle (position t) (direction t) (True) (col t)

-- | Changes the color of the turtle's pen.
-- color :: Color -> Program
-- color c turtle = turtle ++ [(Col, (newTurtle, t))]
        -- where
           -- t = fst . snd $ last turtle
           -- newTurtle = Turtle (position t) (direction t) (pen t) (c)

-- | "Kills" the turtle.
-- die :: Program
-- die turtle = turtle ++ [(Die, (newTurtle, t))]
        -- where
           -- t = fst . snd $ last turtle
           -- newTurtle = Turtle (position t) (direction t) (pen t) (col t)

-- | A program that does nothing.
idle :: Program
idle turtle = turtle ++ [(Op Idle)]
           

-- | Makes the turtle stop what it is doing after a specified period of time.
limited :: Int -> Program
limited = undefined

-- | Kills the turtle after a specified period of time.
lifespan :: Int -> Program
lifespan n p = p ++ [Op (Life n)]

-- | Turtle take a step back.
backward  :: Double -> Program
backward length turtle = forward (-length) turtle

-- | Turn the turtle left with given angle.
left :: Double -> Program 
left angle turtle = right (-angle) turtle

-- | Repeats a turtle program a certain number of times.
times :: Int -> Program
times n p = [(TM (head(p)) n)]

-- | Repeats a program forever.
forever :: [Action] -> [Action] -> [Action]
forever turtle prog = prog ++ [(F (head(turtle)))]


updatePosTurtle :: Turtle -> Double -> Turtle
updatePosTurtle t length = Turtle (newPos) (direction t) (pen t) (col t)
                    where
                       (x,y) = position t
                       angle = direction t
                       newPos = (x + length * cos (angle * pi / 180), 
                                 y + length * sin (angle * pi / 180))


updateDegTurtle :: Turtle -> Double -> Turtle
updateDegTurtle t angle = Turtle (position t) (newDir) (pen t) (col t)
                    where
                       newDir = direction t + angle
               








