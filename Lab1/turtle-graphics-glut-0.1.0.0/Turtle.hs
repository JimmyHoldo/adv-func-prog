-- | proper module documentation here
module Turtle (
  -- * The turtle type(s)
  -- Non-exhaustive list of possible types: Turtle, Program, Action, Operation ...
  Program
  , Turtle (..)
  -- , Action
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
  , limited
  , lifespan
  , backward
  , left
  --, times
  , forever
  , startTurtle
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

--data Action = A(Operation, Turtle, Turtle) 

data Operation = Start | Move | Turn | Idle | Col | Penup | Pendown | Die
            deriving Show

data Turtle = Turtle { 
      position  :: Position
    , direction :: Angle
    , pen       :: Bool
    , col       :: Color
    }     
  deriving Show

startTurtle = (Start, (t, t)):[]
        where t = Turtle (300,300) 0 True (0,255,255)

--  You can use newtype instead of data if you wish.
-- Should be more than a turtle, perhaps action and operation
type Program = [(Operation, (Turtle, Turtle)) ] -> [(Operation, (Turtle, Turtle)) ]

-- | Turtle take a step forward.
forward  :: Double -> Program
forward length turtle = turtle ++ [(Move, (newTurtle, t))]
        where 
            t = fst . snd $ last turtle
            (x,y) = position t
            angle = direction t
            newPos = (x + length * cos (angle * pi / 180), 
                               y + length * sin (angle * pi / 180))
            newTurtle = Turtle (newPos) (direction t) (pen t) (col t)
            
-- | Turn the turtle right with given angle.
right :: Double -> Program 
right angle turtle = turtle ++ [(Turn, (newTurtle, t))]
        where 
            t = fst . snd $ last turtle
            newDir = direction t + angle
            newTurtle = Turtle (position t) (newDir) (pen t) (col t)
            
-- | Perform commands one after another.
(>*>):: t1 -> (t -> t1 -> t2) -> t -> t2
(>*>) turtle1 f n = f n turtle1

-- | Penup: stop drawing turtle
penup :: Program
penup turtle = turtle ++ [(Penup, (newTurtle, t))]
        where
           t = fst . snd $ last turtle
           newTurtle = Turtle (position t) (direction t) (False) (col t)
-- | Pendown: start drawing turtle
pendown :: Program
pendown turtle = turtle ++ [(Pendown, (newTurtle, t))]
        where
           t = fst . snd $ last turtle
           newTurtle = Turtle (position t) (direction t) (True) (col t)

-- | Changes the color of the turtle's pen.
color :: Color -> Program
color c turtle = turtle ++ [(Col, (newTurtle, t))]
        where
           t = fst . snd $ last turtle
           newTurtle = Turtle (position t) (direction t) (pen t) (c)

-- | "Kills" the turtle.
die :: Program
die turtle = turtle ++ [(Die, (newTurtle, t))]
        where
           t = fst . snd $ last turtle
           newTurtle = Turtle (position t) (direction t) (pen t) (col t)

-- | A program that does nothing.
idle :: Program
idle turtle = turtle ++ [(Idle, (t, t))]
           where 
              t = fst . snd $ last turtle

-- | Makes the turtle stop what it is doing after a specified period of time.
limited :: Int -> Program
limited = undefined

-- | Kills the turtle after a specified period of time.
lifespan :: Int -> Program
lifespan = undefined

-- | Turtle take a step back.
backward  :: Double -> Program
backward length turtle = forward (-length) turtle

-- | Turn the turtle left with given angle.
left :: Double -> Program 
left angle turtle = right (-angle) turtle

-- | Repeats a turtle program a certain number of times.
-- times :: Int -> Program
-- times n turtle = case last turtle of
                 -- (Move, (t1,t2)) -> [nextTurtle ]

-- | Repeats a program forever.
forever :: Program
forever = undefined





















