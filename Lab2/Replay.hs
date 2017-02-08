module Lab2 where
import Control.Monad       (liftM, ap)

--types
type Trace r = [Item r]
data Item r = Answer r | Result String
  deriving (Show,Read)
data Replay q r a where
-- Constructors
  Io     :: (Show a, Read a) => IO a -> Replay q r a
  Ask    :: q -> Replay q r r
  Return :: q r a -> Replay q r a
  --(>>=)  :: a -> (a -> Replay b) -> Replay b

emptyTrace :: Trace r
emptyTrace = []

addAnswer  :: Trace r -> r -> Trace r
addAnswer t r = t ++ [Answer r]

-- Run function
-- run :: Replay q r a -> Trace r -> IO (Either (q, Trace r) a)

instance Show r => Show (Trace r)
instance Read r => Read (Trace r)
instance Monad (Replay q r) where
  return = Return
--  (>>=)  =
