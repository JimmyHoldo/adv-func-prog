module Main where

import Data.IORef
import Replay
import System.Exit

-- | Runs the test suite for the replay library
main :: IO ()
main = do
  results <- runTests
  if and results
    then return ()
    else exitFailure

-- | Programs are parameterised over a 'tick' action.
--   Questions are () and answers are integers.
type Program = IO () -> Replay () Int Int

-- | A result is a pair of the final result of the program
--   and the number of 'ticks' executed.
type Result  = (Int, Int)
type Input   = [Int]

-- | A test case collects a program and a list of answers together
--   with its expected result.
data TestCase = TestCase
  { testName    :: String
  , testInput   :: Input
  , testResult  :: Result
  , testTrace   :: Trace Int
  , testProgram :: Program
  }

-- | Running a program.
runProgram :: Program -> Input -> Trace Int -> IO Result
runProgram p inp tr = do
    counter <- newIORef 0
    let tick = modifyIORef counter (+1)
    x <- play (p tick) tr inp
    n <- readIORef counter
    return (x, n)
  where
    play prog t inp = do
      r <- run prog t
      case r of
        Right x      -> return x
        Left (_, t') -> case inp of
          []       -> error "too few inputs"
          a : inp' -> play prog (addAnswer t' a) inp'

-- | Checking a test case. Compares expected and actual results.
checkTestCase :: TestCase -> IO Bool
checkTestCase (TestCase name i r tr p) = do
  putStr $ name ++ ": "
  r' <- runProgram p i tr
  if r == r'
    then putStrLn "ok" >> return True
    else putStrLn ("FAIL: expected " ++ show r ++
                  " instead of " ++ show r')
         >> return False


-- | List of interesting test cases.
testCases :: [TestCase]
testCases =
  [ TestCase
    { testName    = "test1"
    , testInput   = [3,4]
    , testResult  = (8, 1)
    , testTrace   = emptyTrace
    , testProgram = \tick -> do
        io tick
        a <- ask () -- should be 3
        b <- io (return 1)
        c <- ask () -- should be 4
        return (a + b + c)
    }
  , TestCase
    { testName    = "test2"
    , testInput   = []
    , testResult  = (8, 1)
    , testTrace   = [(Answer 3), (Result "1"), (Answer 4)]
    , testProgram = \tick -> do
        io tick
        a <- ask () -- should be 3
        b <- io (return 1)
        c <- ask () -- should be 4
        return (a + b + c)
    }
    , TestCase
      { testName    = "test3"
      , testInput   = [4]
      , testResult  = (8, 1)
      , testTrace   = [(Answer 3), (Result "1")]
      , testProgram = \tick -> do
          io tick
          a <- ask () -- should be 3
          b <- io (return 1)
          c <- ask () -- should be 4
          return (a + b + c)
      }
      , TestCase
        { testName    = "test4"
        , testInput   = [4]
        , testResult  = (8, 1)
        , testTrace   = [(Answer 3)]
        , testProgram = \tick -> do
            io tick
            a <- ask () -- should be 3
            b <- io (return 1)
            c <- ask () -- should be 4
            return (a + b + c)
        }
        , TestCase
          { testName    = "test5"
          , testInput   = [4, 5]
          , testResult  = (9, 2)
          , testTrace   = emptyTrace
          , testProgram = \tick -> do
              io tick
              a <- ask ()
              io tick
              c <- ask ()
              return (a + c)
          }
          , TestCase
            { testName    = "test6"
            , testInput   = [2,2]
            , testResult  = (12, 0)
            , testTrace   = [(Answer 3)]
            , testProgram = \tick -> do
                a <- ask () -- should be 3
                b <- do b <- ask ()
                        c <- ask ()
                        return (b+c)
                return (a * b)
            }
            , TestCase
              { testName    = "test7"
              , testInput   = [4]
              , testResult  = (11, 0)
              , testTrace   = [(Answer 3)]
              , testProgram = \tick -> do
                  a <- ask () -- should be 3
                  b <- ask () -- should be 4
                  return (a + b*2)
              }
  ]

-- | Running all the test cases.
runTests = mapM checkTestCase testCases
