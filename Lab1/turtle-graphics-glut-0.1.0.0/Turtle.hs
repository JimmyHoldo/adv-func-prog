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
  , (<|>)
  -- , ...
  , penup
  , pendown
  , color
  , die
  , idle
  , limited
  , lifespan
  , times
  , forever
  -- * Derived operations
  , backward
  , left
  -- ...
  , updatePosT
  , updateDegT
  , updateTLife
  , updateTLimit
  , updatePenT
  , updateColT
  , decreaseTLifeLimit
  , getTurtle
  , updateBranchTurtle
  -- * Run functions
  , runTextual
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
      position    :: Position
    , direction   :: Angle
    , pen         :: Bool
    , col         :: Color
    , life        :: Int
    , dead        :: Bool
    , branch      :: String
    , branchDepth :: Int
    , limit     :: Int
    }     
  deriving Show


--  You can use newtype instead of data if you wish.
-- Should be more than a turtle, perhaps action and operation
data Action = Op Operation | Seq Action Action | Par Action Action |
              ForE Action | Times Action Int | Limit Action Int
        deriving Show

type Program = Action 


-- | Turtle take a step forward.
forward  :: Double -> Program
forward length = (Op (Move length))

            
-- | Turn the turtle right with given angle.
right :: Double -> Program 
right deg = (Op (Turn deg))

            
-- | Perform commands one after another
(>*>) :: Program -> Program -> Program
t1 >*> t2 = (Seq (t1) (t2))

-- | Perform commands in parallel 
(<|>) :: Program -> Program -> Program
(<|>) p1 p2 = (Par p1 p2)

-- | Penup: stop drawing turtle
penup :: Program
penup = (Op (Pen (False)))

-- | Pendown: start drawing turtle
pendown :: Program
pendown = (Op (Pen (True)))

-- | Changes the color of the turtle's pen.
color :: Color -> Program
color c = (Op (Col (c)))

-- | "Kills" the turtle.
die :: Program
die = (Op (Die))

-- | A program that does nothing.
idle :: Program
idle = (Op (Idle))
           

-- | Makes the turtle stop what it is doing after a specified period of time.
limited :: Int -> Program -> Program
limited n prog = (Limit prog n)

-- | Kills the turtle after a specified period of time.
lifespan :: Int -> Program
lifespan n = (Op (Life n))

-- | Turtle take a step back.
backward  :: Double -> Program
backward length = forward (-length)

-- | Turn the turtle left with given angle.
left :: Double -> Program 
left angle = right (-angle)

-- | Repeats a turtle program a certain number of times.
times :: Int -> Program -> Program
times n p = (Times (p) n)

-- | Repeats a program forever.
forever :: Program -> Program
forever turtle = (ForE (turtle))

-- | Update a turtles position.
updatePosT :: Double -> Turtle -> Turtle
updatePosT length t = Turtle (newPos) (direction t) (pen t) (col t) (life t) 
                             (dead t) (branch t) (branchDepth t) (limit t)
            where
               (x,y) = position t
               angle = direction t
               newPos = (x + length * cos (angle * pi / 180), 
                         y + length * sin (angle * pi / 180))

-- | Update the angle of the turtle
updateDegT :: Double -> Turtle -> Turtle
updateDegT angle t = Turtle (position t) (newDir) (pen t) (col t) (life t) 
                                 (dead t) (branch t) (branchDepth t) (limit t)
            where newDir = direction t + angle
                       
-- | Update the life of the turtle.               
updateTLife :: Int -> Turtle -> Turtle
updateTLife n t = Turtle (position t) (direction t) (pen t) (col t) (n) 
                              (dead t) (branch t) (branchDepth t) (limit t)
                    

-- | Decrease the life an limit of a turtle.
decreaseTLifeLimit :: Int -> Turtle -> Turtle
decreaseTLifeLimit n t = Turtle (position t) (direction t) (pen t) 
                                (col t) (newLife) (dead t) 
                                (branch t) (branchDepth t) (newLimit)
            where newLife  | (life t) < 0 = -1
                           | (life t) >= 0 && ((life t) - n) < 0 = 0
                           | otherwise = (life t) - n
                  newLimit | (limit t) < 0 = -1
                           | (limit t) >= 0 && ((limit t) - n) < 0 = 0
                           | otherwise = (limit t) - n
                                  
-- | Update the limit of the turtle.               
updateTLimit :: Int -> Turtle -> Turtle
updateTLimit n t = Turtle (position t) (direction t) (pen t) (col t) (life t) 
                          (dead t) (branch t) (branchDepth t) (n)
                    

-- | Update if the the turtle draw or not.                    
updatePenT :: Bool -> Turtle -> Turtle
updatePenT b t = Turtle (position t) (direction t) (b) (col t) (life t) 
                             (dead t) (branch t) (branchDepth t) (limit t)

-- | Update the color of the turtle.
updateColT :: Color -> Turtle -> Turtle
updateColT c t = Turtle (position t) (direction t) (pen t) (c) (life t) 
                             (dead t) (branch t) (branchDepth t) (limit t)

-- | Update the turtles branch info.
updateBranchTurtle :: String -> Int -> Turtle -> Turtle
updateBranchTurtle str n t = Turtle (position t) (direction t) (pen t) 
                                    (col t) (life t) (dead t) (str) 
                                    ((branchDepth t) + n) (limit t)

-- | Get the turtle after a program has run.
getTurtle :: Program -> Turtle -> Turtle
getTurtle (Op (Die))    t = Turtle (position t) (direction t) (pen t) (col t) 
                                   (0) (True) (branch t) (branchDepth t) (0)
getTurtle (Op (Idle))   t = decreaseTLifeLimit 1 t
getTurtle (Op (Life n)) t = updateTLife n t
getTurtle (Limit a n)   t = newT2
            where newT  = getTurtle a (updateTLimit n t)
                  newT2 = updateTLimit (-1) newT
getTurtle (Op (Move n)) t = if (limit t) >= 0 && ((limit t) - 2) < 0
                              then decreaseTLifeLimit 2 t
                              else decreaseTLifeLimit 2 (updatePosT n t)
getTurtle (Op (Turn n)) t = if (limit t) == 0 
                              then decreaseTLifeLimit 1 t
                              else decreaseTLifeLimit 1 (updateDegT n t)
getTurtle (Op (Col n))  t = if (limit t) == 0 
                              then decreaseTLifeLimit 1 t
                              else decreaseTLifeLimit 1 (updateColT n t)
getTurtle (Op (Pen n))  t = if (limit t) == 0 
                              then decreaseTLifeLimit 1 t
                              else decreaseTLifeLimit 1 (updatePenT n t)
getTurtle (Seq (a) (b)) t = newT2
            where
                newT  = getTurtle a t
                newT2 = getTurtle b newT

-- | Print information about executed action.
runTextual :: Program -> Turtle -> IO ()
runTextual (Op (Die))    t = putStrLn $ fixedStartStr t 
                             ++ " The program kills it self"
runTextual (Op (Idle))   t = putStrLn $ fixedStartStr t  
                             ++ " The program idles"
runTextual (Op (Life n)) t = putStrLn $ fixedStartStr t  
                             ++ " The turtle is given " 
                             ++ (show n) ++ " lives"
runTextual (Limit a n) t   = putStrLn $ fixedStartStr t  
                             ++ " The turtle is given " 
                             ++ (show n) ++ " in limit value"
runTextual (Op (Pen n))  t = putStrLn $ fixedStartStr t  
                             ++ " The turtles pen is set to " ++ boolToString n
runTextual (Op (Col c))  t = putStrLn $ fixedStartStr t 
                             ++ " The turtle changes color to " ++ show c
runTextual (Op (Turn n)) t = putStrLn $ fixedStartStr t 
                             ++ " The turtle turns " ++ show n ++ " degrees"
runTextual (Op (Move n)) t = putStrLn $ fixedStartStr t
                             ++ " Move a step with length " ++ (show n)
runTextual (ForE (a))    t = putStrLn $ fixedStartStr t
                             ++ " The program runs forever"
runTextual (Times (a) n) t = putStrLn $ fixedStartStr t
                             ++ " The program has " ++ show n 
                             ++ " times left to run"



-- | Start string for runTextual.
fixedStartStr t = (branch t) ++ (depth(branchDepth t))

-- | If the depth is bigger then zero, the display it.                                        
depth n | n == 0 = ""
        | otherwise = show n

-- | Convert a boolean to string
boolToString :: Bool -> String
boolToString True = "True"
boolToString False = "False"