-- | proper module documentation here
module Turtle (
  -- * The turtle type(s)
  -- Non-exhaustive list of possible types: Turtle, Program, Action, Operation ...
  Program
  , Turtle (..)
  --  Action, Operation

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
  , limited
  , lifespan
  , backward
  , left
  , times
  , forever
  -- * Derived operations
  -- ...

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

data Turtle = Turtle { 
      position  :: Position
    , direction :: Angle
    , pen       :: Bool
    , col       :: Color
    }     
  deriving Show


--  You can use newtype instead of data if you wish.
-- Should be more than a turtle, perhaps action and operation
type Program = Turtle

-- | Turtle take a step forward.
forward  :: Program -> Double -> Program
forward turtle length = Turtle (newPos) (direction turtle) (pen turtle) (col turtle)
        where 
            (x,y) = position turtle
            angle = direction turtle
            newPos = (x + length * cos(angle), y + length * sin(angle))

-- | Turn the turtle right with given angle.
right :: Program -> Double -> Program 
right turtle angle = Turtle (position turtle) (newDir) (pen turtle) (col turtle)
        where 
            newDir = direction turtle + angle
            
-- | Perform commands one after another.
(>*>) :: Program -> Program -> Program
(>*>) = undefined

-- | Penup: stop drawing turtle
penup :: Program -> Program
penup turtle = Turtle (position turtle) (direction turtle) (False) (col turtle)
-- | Pendown: start drawing turtle
pendown :: Program -> Program
pendown turtle = Turtle (position turtle) (direction turtle) (True) (col turtle)

-- | Changes the color of the turtle's pen.
color :: Program -> Color -> Program
color turtle color = Turtle (position turtle) (direction turtle) (pen turtle) (color)

-- | "Kills" the turtle.
die :: Program -> Program
die = undefined

-- | A program that does nothing.
idle :: Program -> Program
idle turtle = Turtle (position turtle) (direction turtle) (pen turtle) (col turtle)

-- | Makes the turtle stop what it is doing after a specified period of time.
limited :: Program -> Int -> Program
limited = undefined

-- | Kills the turtle after a specified period of time.
lifespan :: Program -> Int -> Program
lifespan = undefined

-- | Turtle take a step back.
backward  :: Program -> Double -> Program
backward turtle length = forward turtle (-length)

-- | Turn the turtle left with given angle.
left :: Program -> Double -> Program 
left = undefined

-- | Repeats a turtle program a certain number of times.
times :: Program -> Int -> Program
times = undefined

-- | Repeats a program forever.
forever :: Program -> Program
forever = undefined





















