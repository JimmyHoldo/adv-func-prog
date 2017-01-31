-- | A graphical run function for the turtle DSL
--
-- See tutorial starting at: https://www.haskell.org/haskellwiki/OpenGLTutorial1
module TurtleGraphics (runGraphical) where

import Turtle

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
    displayCallback $= display
    setDisplay =<< get windowSize
    mainLoop
  where
    display :: DisplayCallback
    display = do
             clear [ColorBuffer]
             draw p turtle
             flush

-- | Draw the program on the screen.
draw :: Turtle.Program -> Turtle -> IO()
draw p@(Op (Die))    t = runTextual p t
draw p@(Op (Idle))   t = runTextual p t
draw p@(Op (Life n)) t = runTextual p t
draw p@(Op (Pen n))  t = runTextual p t                    
draw p@(Op (Col c))  t = runTextual p t
draw p@(Op (Turn n)) t = runTextual p t
draw p@(Op (Move n)) t = 
        if life t == 0 || dead t == True
        then return ()
        else do 
            setColor (col t)
            let newT = updatePosTurtle n t
                (x,y) = Turtle.position t
                (x1,y1) = Turtle.position newT
            if (pen t == True) && (dead t /= True)
            then do runTextual p newT
                    (line (floor x,floor y) (floor x1,floor y1))
            else return ()
draw p@(Seq (a) (b)) t = do 
        if life t == 0 || dead t == True
        then return ()
        else do
            draw (a) t
            let newT = getTurtle a t
            if life newT == 0 || dead newT == True
            then return ()
            else do draw (b) newT
draw p@(Par (a) (b)) t = do 
        if life t == 0 || dead t == True
        then return ()
        else do
            let newT = updateBranchTurtle "Left branch "  1 t
            draw a newT
            let newT2 = updateBranchTurtle "Right branch "  0 newT
            draw b newT2
draw f@(ForE (a))    t = do 
        runTextual f t
        draw (a) t
        let newT = getTurtle a t
        if life newT == 0 || dead newT == True
        then return ()
        else draw (f) newT
draw p@(Times (a) n) t = do 
        if life t == 0 || dead t == True
        then return ()
        else do runTextual p t
                draw (a) t
                let newT = getTurtle a t
                if n > 1 || dead newT == True
                then do draw (Times (a) (n-1)) newT
                else do return ()



setColor :: (Double, Double, Double) -> IO ()
setColor (r, g, b) = GL.color $ GL.Color3 (rtf r) (rtf g) (rtf b)
  where rtf = realToFrac :: Double -> GLdouble

conv :: (Integral a,Num b) => a -> b
conv = fromIntegral . toInteger

setDisplay :: Size -> IO ()
setDisplay (Size w h) = do
    viewport $= (Position 0 0,Size w h)
    GL.loadIdentity
    GLU.ortho2D 0 (conv w) (conv h) 0

triangleFan :: [(Int,Int)] -> IO ()
triangleFan xys = renderPrimitive TriangleFan $ sequence_
    [ vertex $ Vertex3 (conv x) (conv y) (0 :: GLint)
    | (x,y) <- xys
    ]

line :: (Int,Int) -> (Int,Int) -> IO ()
line (x0,y0) (x1,y1) = renderPrimitive Lines $ do
    vertex $ Vertex3 (conv x0) (conv y0) (0 :: GLint)
    vertex $ Vertex3 (conv x1) (conv y1) (0 :: GLint)


