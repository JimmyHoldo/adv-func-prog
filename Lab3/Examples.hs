{-# LANGUAGE Trustworthy #-}
import MAC.Lattice
import MAC.Labeled
import MAC.MAC
import MAC.Control
import MAC.Core
import MAC.Effects

low :: MAC L (Labeled L String)
low = label "low"

high :: MAC L (Labeled H String)
high = label "high"

eeeee = do l <- runMAC high
           let MkId pass = unRes l
           return pass

example :: IO String
example = do
    putStrLn "Username:"
    username <- getLine
    putStrLn "Password"
    password <- getLine
    user <- (runMAC (label username :: MAC L (Labeled L String)))
    lfile <- runMAC $ (label password :: MAC L (Labeled H String))
            >>= fetchFile user
    let MkId f = unRes lfile
    return f

fetchFile :: Labeled L String -> Labeled H String -> MAC L (Labeled H String)
fetchFile username password = do
    let MkId lpass = unRes (checkUser username)
    let MkId pass = unRes password
    case lpass == pass of
        True  -> joinMAC $ return "Secret file!"
        False -> joinMAC $ return "Public file!"

checkUser :: Labeled L String -> (Labeled H String)
checkUser username = do
    case unRes username of
        (MkId "high") -> MkRes $ MkId "secret"
        (MkId _)      -> MkRes $ MkId "public"
