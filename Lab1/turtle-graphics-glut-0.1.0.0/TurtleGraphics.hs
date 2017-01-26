-- | A graphical run function for the turtle DSL
--
-- See tutorial starting at: https://www.haskell.org/haskellwiki/OpenGLTutorial1
module TurtleGraphics (runGraphical) where

import Turtle
import Control.Concurrent

import qualified Graphics.Rendering.OpenGL.GL as GL
import qualified Graphics.Rendering.OpenGL.GLU as GLU
import Graphics.UI.GLUT hiding (Program)

tt = [(Move,(Turtle {Turtle.position = (100.0,20.0), direction = 0.0, pen = True, col = (0.0,255.0,255.0)},Turtle {Turtle.position = (20.0,100.0), direction = 0.0, pen = True, col = (0.0,255.0,255.0)}))] ++ startTurtle

runGraphical :: [(Operation, (Turtle, Turtle))] -> IO ()
runGraphical p = do
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
             draw p 
             flush

draw :: [(Operation, (Turtle, Turtle)) ] -> IO()
draw [] = return ()    
draw ((Idle, (t1, t2)):xs) = draw xs
draw ((Start, (t1, t2)):xs) = draw xs
draw ((Move, (t1, t2)):xs) = do 
                setColor (col t1)
                let (x,y) = Turtle.position t1
                    (x1,y1) = Turtle.position t2
                if pen t2 == True
                then line (floor x,floor y) (floor x1,floor y1)
                else return ()
                draw xs
draw ((Turn, (_,_)):xs) = draw xs
draw ((Die, (_,_)):xs) = return ()


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


