{-# LANGUAGE GADTs #-}
module Replay (
    Replay(..)
    , Trace (..)
    , Item (..)
    , addAnswer
    , emptyTrace
    , io
    , ask
    , run
    )where

import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)
import Control.Monad.Trans
import Data.Time


-- Types
data Replay q r a where
    Io     :: (Show a, Read a) =>  IO a -> Replay q r a
    Ask    :: q -> Replay q r r
    Return :: a -> Replay q r a
    Bind   :: Replay q r a -> (a -> Replay q r b) -> Replay q r b


type Trace r = [Item r]

data Item r = Answer r| Result String
  deriving (Show,Read)






-- Operations
instance Monad (Replay q r) where
    return = Return
    (>>=) = Bind

instance Applicative (Replay q r) where
    pure = return
    (<*>) = ap

instance Functor (Replay q r) where
    fmap = liftM

-- instance Show r => Show (Trace r)
-- instance Read r => Read (Trace r)


run :: Replay q r a -> Trace r -> IO (Either (q, Trace r) (a))
run x tr =do e <- run' x tr
             case e of
                (Left (q, tr', tr2')) -> return (Left (q, tr'))
                (Right (a, tr', tr2')) -> return (Right (a))

run' :: Replay q r a -> Trace r -> IO (Either (q, Trace r, Trace r) (a, Trace r, Trace r))
run'(Return x)  tr = return (Right (x, tr, []))
run' (Io x)  tr =
    case tr of
        (Result r):trs -> return $ (Right ((read r), trs, [Result r]))
        trs -> do
            r <- x
            return $ (Right ((r), trs, [Result (show r)]))
run' (Ask x) tr =
    case tr of
        (Answer r):trs -> return $ (Right ((r), trs, [Answer r]))
        trs -> return $ (Left (x, trs, []))
run' (Bind ma f) tr = do
    e1 <- run' ma tr
    case e1 of
        (Left (x, tr, tr2)) -> return $ (Left (x, tr, tr2))
        (Right (x, tr, tr2)) -> do
            e <- run' (f x) tr
            case e of
              (Left (y, tr', tr2')) -> return $ (Left (y, tr2++tr', tr2 ++ tr2'))
              (Right (y, tr', tr2')) -> return $ (Right (y, tr', tr2 ++ tr2'))

io  :: (Show a, Read a) => IO a -> Replay q r a
io = Io


ask :: q -> Replay q r r
ask q = Ask q



emptyTrace :: Trace r
emptyTrace = []

addAnswer  :: Trace r -> r -> Trace r
addAnswer t r = t ++ [Answer r]
