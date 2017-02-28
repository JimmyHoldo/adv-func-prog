{-# LANGUAGE OverloadedStrings #-}
module WebForms (
    Web(..)
    , Field(..)
    , Form(..)
    , Answer(..)
    , runWeb
    , lP
    , luP
    ) where

import Web.Scotty
import Data.Monoid
import Data.Text.Lazy as Lazy
import Data.Maybe
import Replay as R
import Control.Monad.IO.Class
import Debug.Trace
import Data.ByteString.Base64 as Base64 (encode, decode)
import Data.ByteString.Char8 as Char8 (pack, unpack)


type Web a = Replay Form Answer a

data Field = QText String | QInt String |
             InfoText String | QDrop [String]
  deriving (Show, Read)

type Form = [Field]
type Answer = [Text]



-- | Run a web program.
runWeb :: Int -> Web Text -> ActionM ()
runWeb i r = do
            tr <- getTrace
            tr' <- if i == 1
                     then do ans <- getAnswer 0
                             return $ R.addAnswer tr ans
                     else return tr
            res <- liftIO $ R.run r tr'
            case trace (show tr') res of
                (Left (q, t')) -> do html $ page q t'
                (Right text)   -> html text

-- | Fetch an input from a web page.
getInput :: Text -> ActionM Text
getInput s = param s `rescue` \ _ -> return ""

-- | Get the answers from the input fields on a web page.
getAnswer :: Int -> ActionM (Answer)
getAnswer i = do ans <- getInput ( lP $ "answer"++ (show i))
                 case (luP ans) of
                    "" -> return $ []
                    s   -> do next <- (getAnswer (i+1))
                              return ([ans]++next)

-- | Get the stored trace from a web page.
getTrace :: ActionM (R.Trace Answer)
getTrace  = do tr <- getInput ( lP $ "trace")
               case trace (luP tr) (luP tr) of
                    ""  -> return R.emptyTrace
                    s   -> do liftIO $ putStrLn $ show tr
                              return ( (read (decodeTrace s)))

-- | Build a web page.
page :: Form -> Trace Answer -> Text
page qs trace = lP $ "<html><body><form method=post>"
                     ++ ((questionToString qs 0))
                     ++ "<input type=submit value=Submit>"
                     ++ "<input type=hidden name=trace value="
                     ++ encodeTrace(show (trace)) ++ "></body></html>"

-- | Build the content that is shown on the web page.
questionToString :: Form -> Int -> String
questionToString []                 i = ""
questionToString ((QText q):qs)     i = (("<p>" ++ q ++ "</p>"
                                          ++ "<p><input name=answer"
                                          ++( show i)++"></p>")
                                          ++  (questionToString qs (i+1)))
questionToString ((QInt q):qs)      i = (("<p>" ++ q ++ "</p>"
                                          ++ "<p><input type=number name=answer"
                                          ++(show i)++"></p>")
                                          ++ (questionToString qs (i+1)))
questionToString ((InfoText q):qs)  i = ("<p style=white-space:pre>" ++ q
                                            ++ "</p>"
                                            ++ (questionToString qs (i)))
questionToString ((QDrop q):qs)     i =
    "<input type=hidden id=id"++(show i)++" name=answer"++(show i)++" value=ls>"
     ++ ("<select id=mySelect"++(show i)
     ++" onchange=update"++(show i)++"()>")
     ++ (optionsS q) ++ "</select>"
     ++ "<script> function update"++(show i)++"() { "
     ++ "var answer=document.getElementById('mySelect"++(show i)++"').value;"
     ++ "document.getElementById('id"++(show i)++"').value = answer;}"
     ++ "</script>" ++ (questionToString qs (i+1))

-- | Helper function for building a dropdown list.
optionsS :: [String] -> String
optionsS []     = ""
optionsS (x:xs) = "<option value="++x++">"++x++"</option>"
                 ++ optionsS xs


-- | Pack a string to text.
lP :: String -> Text
lP str = Lazy.pack str

-- | Unpack a text to string.
luP :: Text -> String
luP txt = Lazy.unpack txt

-- | Encode the trace so information is not lost when stored on a web page.
encodeTrace :: String -> String
encodeTrace tr =  Char8.unpack $ Base64.encode (Char8.pack tr)

-- | Decode a trace.
decodeTrace   :: String -> String
decodeTrace t = case Base64.decode(Char8.pack(t)) of
                    Left _ -> error "Could not decode the trace"
                    Right bstr -> Char8.unpack bstr
