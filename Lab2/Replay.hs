{-# LANGUAGE GADTs #-}
module Replay where
    
import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)   
import Control.Monad.Trans
import Data.Time

    
    

type Trace r = [Item r]

data Item r = Answer r| Result String 
  deriving (Show,Read)


-- Types
type Replay q r a = ReplayT IO q r a
newtype ReplayT m q r a =  ReplayT {runReplayT :: Trace r -> m ((Either q a), Trace r) }






-- Operations
instance (Monad m, Show q, Read q) =>  Monad (ReplayT m q r) where
    return x = ReplayT $ \trace -> return (Right x, trace)
    x >>= f = ReplayT $ \trace -> do
                            (x, trace2) <- runReplayT x trace
                            ret x trace2
        where
            ret (Left qeustion) tr = return (Left qeustion, tr)
            ret (Right a) tr = runReplayT (f a) tr


-- instance Show r => Show (Trace r)
-- instance Read r => Read (Trace r)




--io  :: (Show a, Read a) => IO a -> ReplayT m q r a
io input = ReplayT $ \trace -> 
            case trace of
                [] -> do i <- input
                         return (Right i, (Result (show i)):trace)
                ((Result r):trs) -> return (Right (read r), trace) -- most likely not correct

                
ask :: (Monad m) => q -> ReplayT m q r r
ask question = ReplayT $ \trace -> 
            case trace of
                [] -> return (Left question, trace)
                ((Answer r):trs) -> return (Right r, trace) -- most likely not correct
                
run :: (Monad m) => ReplayT m q r a -> Trace r -> m (Either (q, Trace r) a)
run replay trace = do 
            (x, trace2) <- runReplayT replay trace
            case x of
                Left q -> return $ Left (q, trace2)
                Right a -> return $ Right a


emptyTrace :: Trace r
emptyTrace = []

addAnswer  :: Trace r -> r -> Trace r
addAnswer t r = t ++ [Answer r]


example :: Replay String String Int
example = do
  --t0 <- io getCurrentTime
  io (putStrLn "Hello!")
  age <- ask "What is your age?"
  io (putStrLn ("You are " ++ (show age)))
  name <- ask "What is your name?"
  io (putStrLn (name ++ " is " ++ age ++ " years old"))
  --t1 <- io getCurrentTime
  -- io (putStrLn ("Total time: " ))
  return (read age)

  
running :: Replay String String a -> IO a
running prog = play emptyTrace
 where
  play t = do
    r <- run prog t    -- this is the same prog every time!
    case r of
      Left (q,t2) -> do
        print t2
        putStr ("Question: " ++ q ++ " ")
        r <- getLine
        play (addAnswer t2 r)
      Right x -> do print t
                    return x

 

                          
instance (Monad m, Show q, Read q) => Applicative (ReplayT m q r) where
    pure = return
    (<*>) = ap 

instance (Monad m, Show q, Read q) => Functor (ReplayT m q r) where
    fmap = liftM

