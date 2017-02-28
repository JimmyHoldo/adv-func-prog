{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

import Web.Scotty
import Data.Text.Lazy as Lazy
import Control.Monad.IO.Class
import Data.Monoid
import System.Plugins.Load
import GHC.Paths

main :: IO ()
main = scotty 3000 $ do
    get  "/" (runWeb 0 )
    post "/" (runWeb 1 )

-- | Run a web program.
runWeb :: Int -> ActionM ()
runWeb i = if i == 0
              then html $ page
              else do
                  text <- getFunction
                  name <- createFile text
                  html $ lP $ "<html><body>"++ (name) ++"</body></html>"



-- | Fetch an input from a web page.
getInput :: Text -> ActionM Text
getInput s = param s `rescue` \ _ -> return ""

-- | Get the answers from the input fields on a web page.
getFunction :: ActionM (Text)
getFunction = do ans <- getInput ( lP $ "program")
                 case (luP ans) of
                    "" -> return $ ""
                    s  -> return ans
-- | Build a web page.
page :: Text
page = lP $ "<html><body><form method=post>"
             ++ "<textarea name=program cols=80 rows=20 ></textarea>"
             ++ "<br><input type=submit value=Submit>"
             ++ "</body></html>"

createFile :: Text -> ActionM (String)
createFile txt = do
    let str = luP $ txt
        name = luP $ Prelude.head (Lazy.words txt)
    liftIO $ writeFile (name++".hs") str
    return $ name

--runFunction :: String -> Int
runFunction str = undefined


-- | Pack a string to text.
lP :: String -> Text
lP str = Lazy.pack str

-- | Unpack a text to string.
luP :: Text -> String
luP txt = Lazy.unpack txt
