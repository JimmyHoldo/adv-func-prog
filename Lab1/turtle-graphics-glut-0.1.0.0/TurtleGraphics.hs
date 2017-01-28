-- | A graphical run function for the turtle DSL
--
-- See tutorial starting at: https://www.haskell.org/haskellwiki/OpenGLTutorial1
module TurtleGraphics (runGraphical) where

import Turtle
import Control.Concurrent

import qualified Graphics.Rendering.OpenGL.GL as GL
import qualified Graphics.Rendering.OpenGL.GLU as GLU
import Graphics.UI.GLUT hiding (Program)

t = (Turtle (300,300) 0 True (0,255,255))
c = [(Op (Move 45.0)), (Op (Turn 45.0)), (Op (Move 100.0)), (Op (Turn 20.0)), (Op (Move 45.0))]
b = [Seq (Seq (Op (Move 45.0)) (Op (Turn 90.0))) (Op (Move 15.0)), Seq (Seq (Op (Move 30.0)) (Op (Turn 90.0))) (Op (Move 45.0))]
a = [Seq (Seq (Seq (Seq (Op (Move 4.0)) (Op (Move 4.0))) (Op (Move 5.0))) (Op (Move 5.0))) (Op (Move 7.0))]

-- | Run graphics
runGraphical :: [Action] -> Turtle -> IO ()
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
             putStrLn (show p)
             draw p turtle
             flush

-- | Draw the program on the screen.
draw :: [Action] -> Turtle -> IO()
draw [] _ = return () 
draw ((Op (Die)):xs) t =do 
        let newT = updateTurtleLife 0 t
        draw xs newT
        return ()
draw ((Op (Idle)):xs) t = do 
        if life t == 0
        then return ()
        else do
            let newT = decreaseTurtleLife 1 t
            draw xs newT
draw ((Op (Life n)):xs) t = do 
        let newT = updateTurtleLife n t
        draw xs newT
        return ()
draw (a@(Op (Pen n)):xs) t = do 
        if life t == 0
        then return ()
        else do let newT = getTurtle a t
                draw xs newT
                return ()
draw (a@(Op (Col c)):xs) t = do 
        if life t == 0
        then return ()
        else do let newT = getTurtle a t
                draw xs newT
                return ()
draw (a@(Op (Turn n)):xs) t = do
        if life t == 0
        then return ()
        else do 
            let newT = getTurtle a t
            draw xs newT
            return ()
draw (a@(Op (Move n)):xs) t = do 
        if life t == 0
        then return ()
        else do
            draw' a t
            let newT = getTurtle a t
            if life t == 0
            then return ()
            else do
                draw xs newT
                return ()
draw (s@(Seq (a) (b)):xs) t = do 
        if life t == 0
        then return ()
        else do
            draw ([a]) t
            let newT = getTurtle a t
            if life newT == 0
            then return ()
            else do draw ([b]) newT
                    let newT2 = getTurtle b newT
                    if life newT2 == 0
                    then return ()
                    else do draw xs newT2
                            return ()
draw (f@(F (a)):xs) t = do 
        draw ([f]) t
        let newT = getTurtle a t
        draw xs newT
        return ()
draw ((TM (a) n):xs) t = do 
        if life t == 0
        then return ()
        else do draw (a) t
                let newT = getTurtle2 a t
                if n > 1 
                then do draw ((TM (a) (n-1)):xs) newT
                        return ()
                else do if life newT == 0
                        then return ()
                        else do draw xs newT
                                return ()
            where
                getTurtle2 (x:xs) tur = getTurtle2 (xs) (getTurtle x tur)
                getTurtle2 [] tur   = tur
                
-- | Helper function for drawing   
draw' :: Action -> Turtle -> IO()
draw' (Op (Move n)) t = do
        setColor (col t)
        let newT = updatePosTurtle n t
            (x,y) = Turtle.position t
            (x1,y1) = Turtle.position newT
        if pen t == True
        then line (floor x,floor y) (floor x1,floor y1)
        else return ()
-- draw' (Op (Turn n)) t = return ()
-- draw' (Op (Die)) t = return ()
-- draw' (Seq (a) (b)) t = do 
        -- if life t == 0
        -- then return ()
        -- else do draw' a t
                -- let newT = getTurtle a t
                -- if life t == 0
                -- then return ()
                -- else do draw' b newT
-- draw' (F (a)) t = do 
                -- draw' a t
                -- return ()
-- draw' (TM (a) n) t = do 
                -- draw' a t 
                -- return ()

-- | Get the turtle after a program has run.
getTurtle :: Action -> Turtle -> Turtle
getTurtle (Op (Die))    t = updateTurtleLife 0 t
getTurtle (Op (Move n)) t = decreaseTurtleLife 2 (updatePosTurtle n t)
getTurtle (Op (Turn n)) t = decreaseTurtleLife 1 (updateDegTurtle n t)
getTurtle (Op (Col n))  t = decreaseTurtleLife 1 (updateTurtleCol n t)
getTurtle (Op (Pen n))  t = decreaseTurtleLife 1 (updateTurtlePen n t)
getTurtle (Seq (a) (b)) t = newT2
            where
                newT  = getTurtle a t
                newT2 = getTurtle b newT
getTurtle (F (a)) t = getTurtle a t
getTurtle (TM (a) n) t = getTurtle2 (a) t
            where
                getTurtle2 (x:xs) tur = getTurtle2 (xs) (getTurtle x tur)
                getTurtle2 [] tur   = tur





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


