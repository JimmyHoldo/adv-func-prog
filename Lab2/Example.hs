module Example where

import Data.Time
import Replay

main = running emptyTrace example

example :: Replay String String Int
example = do
  t0 <- io getCurrentTime
  io (putStrLn "Hello!")
  age <- ask "What is your age?"
  io (putStrLn ("You are " ++ (show age)))
  name <- ask "What is your name?"
  io (putStrLn (name ++ " is " ++ age ++ " years old"))
  t1 <- io getCurrentTime
  io (putStrLn ("Total time: " ++ show (diffUTCTime t1 t0)))
  return (read age)


running :: Trace String -> Replay String String a -> IO a
running tr prog = play tr
 where
  play t = do
    r <- run prog t    -- this is the same prog every time!
    case r of
      Left (q,t2) -> do
        --print t2
        putStr ("Question: " ++ q ++ " ")
        r <- getLine
        play (addAnswer t2 r)
      Right x -> do print t
                    return x
