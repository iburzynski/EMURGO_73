{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use >>" #-}
module Main where
import Lib

-- *** Bind (>>=), Sequence (>>) & `return`: Adapters for Performing IO Actions ***
-- Note: these adapters aren't specific to the IO context: they can be used with any monadic context
  -- Here we will look at them specifically in IO

-- Bind (>>=) "pushes" an IO value into a function to yield a new IO value
-- (>>=) :: IO a -> (a -> IO b) -> IO b
-- ex. getLine (:: IO String) >>= putStrLn (:: String -> IO ())
--     ^ gets string from user and returns it in IO context
--                                ^ takes a regular string and prints it to console
-- Note how the (>>=) adapter lets us apply a function expecting a regular String to an IO String!

-- Sequence (>>) performs the IO action on the left, discards its return value and then performs the
  -- IO action on the right.
-- (>>)  :: IO a -> IO b -> IO b
--          ^ the return value of this IO Action is ignored/discarded

-- Sequence (>>) can be defined in terms of bind if we ignore the input to the lambda function and
  -- simply return the second IO value:

  -- ioA >> ioB == ioA >>= (\_ -> ioB)

-- `return` takes a value of any type (`a`) and "promotes" it into the IO context:
-- return :: a -> IO a

-- This is useful for when we need to construct IO values inside our functions, to satisfy the type
  -- requirements of bind.

----------------------------------------------------------------------------------------------------

-- *** Do Notation: Imperative-style Syntax in a Pure Functional World ***
-- `do` blocks provide syntactic sugar for bind (>>=) and sequence (>>)
-- As above, it can be used with any monadic context, but for now we'll look only at IO.

-- Using bind/sequence:
main' :: IO ()
main' =
  putStrLn "Enter the secret number:" >>
  getNumber >>= (\secret -> processGuess secret)

-- Using do-notation:
main :: IO ()
main = do
  -- IO Actions can be written sequentially to be performed and have their return values discarded:
  putStrLn "Enter the secret number:"
  -- There is an implicit call to (>>) at the end of this Action.

  -- The left arrow (<-) "unpacks" a value in the context (IO) and binds it to a variable name.
  secret <- getNumber
  -- The variable can then have ordinary functions applied to it.
  processGuess secret
  -- The last line of the `do` block must be an expression matching the expected type.
  -- Here we opened the `do` block at the top level of the `main` Action, which expects type `IO ()`
    -- so the final expression in our `do` block must also have type `IO ()`

-- Compare the do-notation version to the bind/sequence version above. To GHC they are identical.
-- Try to identify which pieces correspond to eachother in the two versions. It's important not to
-- use `do` syntax blindly: we need to properly understand how it "desugars" into calls to bind and
-- sequence with lambda expressions under the hood.

-- Using bind/sequence:
processGuess' :: Int -> IO ()
processGuess' secret = putStrLn "Enter your guess:" >> getNumber >>= go
  where
    go :: Int -> IO ()
    go g = case checkGuess secret g of
      (msg, True)  -> putStrLn msg -- if guess is correct, print the winning message and terminate
      (msg, False) -> putStrLn msg >> processGuess' secret -- else print the message and loop

-- Using do-notation:
processGuess :: Int -> IO ()
processGuess secret = do
  putStrLn "Enter your guess:"
  guess <- getNumber
  case checkGuess secret guess of
    (msg, True)  -> putStrLn msg
    -- To continue writing in `do`-style syntax within our case expression, we need to open another
    -- `do` block on the right side of the (->):
    (msg, False) -> do
      putStrLn msg
      processGuess secret

-- Using bind/return:
getNumber' :: IO Int
getNumber' = getLine >>= convertToInt
 where
 convertToInt :: String -> IO Int
 convertToInt = (\s -> return (read s))

-- Using do-notation/return:
getNumber :: IO Int
getNumber = do
  n <- getLine
  return $ read n