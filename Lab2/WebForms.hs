{-# LANGUAGE OverloadedStrings #-}
module WebForms (

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

data Field = QText String | QInt String
  deriving (Show, Read)

type Form = [Field]
type Answer = [Text]

lP :: String -> Text
lP str = Lazy.pack str

luP :: Text -> String
luP txt = Lazy.unpack txt

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

main :: IO ()
main = scotty 3000 $ do
    get  "/" (runWeb 0 example)
    post "/" (runWeb 1 example)


runWeb :: Int -> Web Text -> ActionM ()
runWeb i r = do
            tr <- getTrace
            tr' <- if i == 1
                     then do ans <- getAnswer 0
                             return $ R.addAnswer tr ans
                     else  return tr
            res <- liftIO $ R.run r tr'
            case trace (show tr') res of
                (Left (q, t')) -> do html $ page q t'
                (Right text)   -> html text


getInput :: Text -> ActionM Text
getInput s = param s `rescue` \ _ -> return ""

getAnswer :: Int -> ActionM (Answer)
getAnswer i = do ans <- getInput ( lP $ "answer"++ (show i))
                 case (luP ans) of
                    "" -> return $ []
                    s   -> do next <- (getAnswer (i+1))
                              return ([ans]++next)

getTrace :: ActionM (R.Trace Answer)
getTrace  = do tr <- getInput ( lP $ "trace")
               case trace (luP tr) (luP tr) of
                    ""  -> return R.emptyTrace
                    s   -> do liftIO $ putStrLn $ show tr
                              return ( (read (decodeTrace s)))

page :: Form -> Trace Answer -> Text
page qs trace = lP $ "<html><body><form method=post>"
                     ++ ((questionToText qs 0))
                     ++ "<input type=submit value=Submit>"
                     ++ "<input type=hidden name=trace value="
                     ++ encodeTrace(show (trace)) ++ "></body></html>"

questionToText :: Form -> Int -> String
questionToText []             i = ""
questionToText ((QText q):qs) i = (("<p>" ++ q ++ "</p>"
                                  ++ "<p><input name=answer"
                                  ++( show i)++"></p>")
                                  ++  (questionToText qs (i+1)))
questionToText ((QInt q):qs)  i = (("<p>" ++ q ++ "</p>"
                                  ++ "<p><input type=number name=answer"
                                  ++(show i)++"></p>")
                                  ++ (questionToText qs (i+1)))



encodeTrace :: String -> String
encodeTrace tr =  Char8.unpack $ Base64.encode (Char8.pack tr)


decodeTrace   :: String -> String
decodeTrace t = case Base64.decode(Char8.pack(t)) of
                    Left _ -> "Fail"
                    Right bstr -> Char8.unpack bstr
