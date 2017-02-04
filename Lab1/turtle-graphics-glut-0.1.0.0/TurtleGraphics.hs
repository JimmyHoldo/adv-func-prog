-- | A graphical run function for the turtle DSL
--
-- See tutorial starting at: https://www.haskell.org/haskellwiki/OpenGLTutorial1
module TurtleGraphics (runGraphical) where

import Turtle
import Control.Concurrent
import TurtleExtras

import qualified Graphics.Rendering.OpenGL.GL as GL
import qualified Graphics.Rendering.OpenGL.GLU as GLU
import Graphics.UI.GLUT hiding (Program)

-- | Run graphics
runGraphical :: Turtle.Program -> Turtle -> IO ()
runGraphical p turtle = do
    initialWindowSize $= Size 600 600
    (_progName, _args) <- getArgsAndInitialize
    window <- createWindow "Turtle!"
    reshapeCallback $= Just setDisplay
    clear [ColorBuffer]
    displayCallback $= draw p turtle--display
    setDisplay =<< get windowSize
    mainLoop
  where
    display :: DisplayCallback
    display = return ()

-- | Draw the program on the screen.
draw :: Turtle.Program -> Turtle -> IO()
draw p@(Op (Die))    t = runTextual p t
draw p@(Op (Idle))   t = runTextual p t
draw p@(Op (Life n)) t = runTextual p t
draw p@(Op (Pen n))  t = runTextual p t                    
draw p@(Op (Col c))  t = runTextual p t
draw p@(Op (Turn n)) t = runTextual p t
draw p@(Limit a n) t = do
        let newT = updateTLimit n t
        runTextual p newT
        draw a newT
draw p@(Op (Move n)) t = 
        if life t == 0 || dead t == True || limit t == 0
        then return ()
        else do 
            setColor (col t)
            let newT = updatePosT n t
                (x,y) = Turtle.position t
                (x1,y1) = Turtle.position newT
            if pen t == True
              then do
                 runTextual p newT
                 (line (floor x,floor y) (floor x1,floor y1))
                 flush
              else runTextual p newT

draw p@(Seq (a) (b)) t = 
        if life t == 0 || dead t == True || limit t == 0
          then return ()
          else do
              draw a t
              let newT = getTurtle a t
              if life newT == 0 || dead newT == True 
                then return ()
                else do draw (b) newT
draw p@(Par (a) (b)) t = 
        if life t == 0 || dead t == True || limit t == 0
          then return ()
          else do
              let newT = updateBranchTurtle "Left"  1 t
              let newT2 = updateBranchTurtle "Right"  0 newT
              draw' (getFirstInstruction p) newT newT2
draw f@(ForE (a))    t = do 
        runTextual f t
        draw (a) t
        let newT = getTurtle a t
        if life newT == 0 || dead newT == True || limit newT == 0
          then return ()
          else draw (f) newT
draw p@(Times (a) n) t = 
        if life t == 0 || dead t == True || limit t == 0
          then return ()
          else do runTextual p t
                  draw (a) t
                  let newT = getTurtle a t
                  if n > 1 || dead newT == True || limit newT == 0
                    then draw (Times (a) (n-1)) newT
                    else return ()
                    
                 

-- | Helper function for drawing parallel
draw' (a@(Times p1 n1), b@(Times p2 n2), (Empty)) t t2 = 
        if n1 > 0
          then if life t == 0 || dead t == True || limit t == 0
                then return ()
                else do
                  let prog1 = getFirstInstruction' p1
                  draw (prog1) t
                  if (life t2 == 0 || dead t2 == True || limit t2 == 0) && n2 /= 0
                    then return ()
                    else do
                      let prog2 = getFirstInstruction' p2
                      draw (prog2) t2
                      let newT = getTurtle prog1 t
                      let newT2 = getTurtle prog1 t2
                      let prog3 = getRest p1 
                          prog4 = getRest p2 
                      draw' (getFirstInstruction (Par prog3 prog4))
                            (newT) (newT2) 
                      draw' ((Times p1 (n1-1)), (Times p2 (n2-1)), Empty) 
                              (getTurtle prog3 newT) (getTurtle prog4 newT2)
          else return ()
            
draw' (a, b, (Empty)) t t2 = do
        if life t == 0 || dead t == True || limit t == 0
          then return ()
          else draw a t
        if life t2 == 0 || dead t2 == True || limit t2 == 0
          then return ()
          else draw b t2
draw' (a, b, p@(Par (Seq _ _) (Seq _ _))) t t2 = do
        if life t == 0 || dead t == True || limit t == 0
          then return ()
          else do
            draw a t
        if life t2 == 0 || dead t2 == True || limit t2 == 0
          then return ()
          else do
            draw b t2
        let newT = getTurtle a t
        let newT2 = getTurtle b t2
        draw' (getFirstInstruction p) newT newT2
draw' (a, b, p@(Par p1@(Times _ n) p2@(Times _ n1))) t t2 = do
        if life t == 0 || dead t == True || limit t == 0
          then return ()
          else do
            draw a t
        if life t2 == 0 || dead t2 == True || limit t2 == 0
          then return ()
          else do
            draw b t2
        let newT = getTurtle a t
        let newT2 = getTurtle b t2
        draw' (getFirstInstruction p) newT newT2
draw' (a, b, (Par p@(Par _ _) p1@(Seq _ _))) t t2 = do
        if life t == 0 || dead t == True || limit t == 0
          then return ()
          else do
            draw a t
            let newT = getTurtle a t
            draw p newT
        if life t2 == 0 || dead t2 == True || limit t2 == 0
          then return ()
          else do
            draw b t2
            let newT2 = getTurtle b t2
            draw p1 newT2
draw' (a, b, (Par p@(Seq _ _) p1@(Par _ _))) t t2 = do
        if life t == 0 || dead t == True || limit t == 0
          then return ()
          else do
            draw a t
            let newT = getTurtle a t
            draw p newT
        if life t2 == 0 || dead t2 == True || limit t2 == 0
          then return ()
          else do 
            draw b t2
            let newT2 = getTurtle b t2
            draw p1 newT2
draw' (a, b, (Par p@(Seq _ _) s2)) t t2 = 
        if life t == 0 || dead t == True || limit t == 0
          then return ()
          else do
            draw a t
            draw b t2
            let newT = getTurtle a t
            let newT2 = getTurtle b t2
            draw p newT
            draw s2 newT2
draw' (a, b, (Par (s1) p@(Seq _ _))) t t2 = do
        if life t == 0 || dead t == True || limit t == 0
          then return ()
          else do
            draw a t
            let newT = getTurtle a t
            draw s1 newT
        if life t2 == 0 || dead t2 == True || limit t2 == 0
          then return ()
          else do
            draw b t2
            let newT2 = getTurtle b t2
            draw p newT2
draw' (a, b, (Par p@(Par _ _) s2)) t t2 = do
        if life t == 0 || dead t == True || limit t == 0
          then return ()
          else do
            draw a t
            let newT = getTurtle a t
            draw p newT
        if life t2 == 0 || dead t2 == True || limit t2 == 0
          then return ()
          else do
            draw b t2
            let newT2 = getTurtle b t2
            draw s2 newT2
draw' (a, b, (Par (s1) p@(Par _ _))) t t2 = do
        if life t == 0 || dead t == True || limit t == 0
          then return ()
          else do
            draw a t
            let newT = getTurtle a t
            draw s1 newT
        if life t2 == 0 || dead t2 == True || limit t2 == 0
          then return ()
          else do
            draw b t2
            let newT2 = getTurtle b t2
            draw p newT2
draw' (a, b, p@(Par (s1) (s2))) t t2 = do
        if life t == 0 || dead t == True || limit t == 0
          then return ()
          else do
            draw a t
            let newT = getTurtle a t
            draw s1 newT
        if life t2 == 0 || dead t2 == True || limit t2 == 0
          then return ()
          else do
            draw b t2
            let newT2 = getTurtle b t2
            draw s2 newT2
     
              
              
-- | Get the first instruction from both parallel and the remaining 
--   instructions.               
getFirstInstruction :: Program -> (Program, Program, Program)
getFirstInstruction (Par a@(Par (a1) (b1)) b@(Par (a2) (b2)))
                    = (a, b, (Empty))
getFirstInstruction (Par a@(Seq (a1) (b1)) b@(Seq (a2) (b2)))
                    = (ret1, ret2, (Par a' b'))
    where 
        ret1 = getFirstInstruction' a
        ret2 = getFirstInstruction' b
        a' = getRest a
        b' = getRest b
getFirstInstruction (Par a@(Par (a1) (b1)) b@(Seq (a2) (b2)))
                    = (a, b, Empty)
getFirstInstruction (Par a@(Seq (a1) (b1)) b@(Par (a2) (b2)))
                    = (a, b, Empty)       
getFirstInstruction (Par a b@(Par (a2) (b2)))
                    = (a, b, Empty)
getFirstInstruction (Par a@(Par (a1) (b1)) b)
                    = (a, b, Empty)       
getFirstInstruction (Par a b@(Seq (a2) (b2)))
                    = (a, b, Empty)
getFirstInstruction (Par a@(Seq (a1) (b1)) b)
                    = (a, b, Empty)
getFirstInstruction (Par a@(Times _ _) b@(Times _ _))
                    = (a, b, Empty)


        
-- | Get the tail of the instructions.     
getRest :: Program -> Program
getRest i@(Op (Die)) = i
getRest i@(Op (Idle)) = i
getRest i@(Op (Life n)) = i
getRest i@(Op (Pen n)) = i
getRest i@(Op (Col n)) = i
getRest i@(Op (Turn n)) = i
getRest i@(Limit a n) = i
getRest i@(Op (Move n)) = i
getRest (Seq (a@(Seq c d)) (b)) = (Seq (getRest a) b) 
getRest (Seq (a) (b)) = b
getRest (Par (a@(Par c d)) (b)) = (Par (getRest a) b) 
getRest (Par (a@(Seq c d)) (b)) = (Par (getRest a) b) 
getRest (Par (a) (b)) = b
        
        


-- | Get the head of a list of instructions.
getFirstInstruction' :: Program -> Program
getFirstInstruction' i@(Op (Die)) = i
getFirstInstruction' i@(Op (Idle)) = i
getFirstInstruction' i@(Op (Life n)) = i
getFirstInstruction' i@(Op (Pen n)) = i
getFirstInstruction' i@(Op (Col n)) = i
getFirstInstruction' i@(Op (Turn n)) = i
getFirstInstruction' i@(Limit a n) = getFirstInstruction' a
getFirstInstruction' i@(Times a n) = getFirstInstruction' a
getFirstInstruction' i@(Op (Move n)) = i
getFirstInstruction' (Seq (a) (b)) = ret
            where
                ret = getFirstInstruction' a
getFirstInstruction' p@(Par (a) (b)) = p
                
                
                
                
-- | Set the color for drawing.
setColor :: (Double, Double, Double) -> IO ()
setColor (r, g, b) = GL.color $ GL.Color3 (rtf r) (rtf g) (rtf b)
  where rtf = realToFrac :: Double -> GLdouble

-- | Convert number
conv :: (Integral a,Num b) => a -> b
conv = fromIntegral . toInteger

-- | Set the display window.
setDisplay :: Size -> IO ()
setDisplay (Size w h) = do
    viewport $= (Position 0 0,Size w h)
    GL.loadIdentity
    GLU.ortho2D 0 (conv w) (conv h) 0

-- | Draw a line on the screen.
line :: (Int,Int) -> (Int,Int) -> IO ()
line (x0,y0) (x1,y1) = renderPrimitive Lines $ do
    vertex $ Vertex3 (conv x0) (conv y0) (0 :: GLint)
    vertex $ Vertex3 (conv x1) (conv y1) (0 :: GLint)


