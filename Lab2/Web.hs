{-# LANGUAGE OverloadedStrings #-}
module Main where

import Replay
import WebForms
import System.Process
import Web.Scotty
import Data.Text.Lazy as Lazy
import Control.Monad.IO.Class


main :: IO ()
main = scotty 3000 $ do
    get  "/" (runWeb 0 (example2 ("")))
    post "/" (runWeb 1 (example2 ("")))

example :: Web Text
example = do ans1 <- ask $ [QInt ("Year of birth?"), QInt ("Year now?")]
             ans2 <- ask $ [QText ("Name?")]
             return $ lP ("<html><body>"
                ++ (luP $ Prelude.head ans2)
                ++ " is "
                ++ (show((read(luP $ Prelude.head(Prelude.tail ans1)) :: Int)
                    -(read (luP $ Prelude.head ans1) :: Int)))
                ++ "  years old!"
                ++"</body></html>")


example2 :: String -> Web Text
example2 txt = do ans1 <- ask $ [InfoText (txt),
                                 InfoText ("Type of command:"),
                                 QDrop ["ls", "echo", "date"],
                                 QText ("write commands if needed!")]
                  let (x,y,z) = commandAndArg ans1
                  ret <- io $ readProcess x y z
                  ans2 <- ask $ [InfoText (ret),
                                 QInt ("Write 1 to continue")]
                  if Prelude.head ans2 /= "1"
                    then return $ lP ("<html><body>"++ret++"</body></html>")
                    else example2 ret
      where
        commandAndArg (x:y:xs) | ((luP x) == "ls") || ((luP x) == "date")
                                  = ((luP x), [], [])
                               | otherwise = ((luP x), [(luP y)], [])
        commandAndArg (x:xs)   = ((luP x), [], [])
        commandAndArg []   = ((""), [], [])
