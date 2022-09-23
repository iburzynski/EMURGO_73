{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Replace case with maybe" #-}
{-# HLINT ignore "Use <&>" #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use list comprehension" #-}
{-# HLINT ignore "Use concatMap" #-}
{-# HLINT ignore "Redundant bracket" #-}

import qualified Data.Map.Strict as M
import Data.Char (toUpper)

-- *** Do-notation Refresher ***
-- We saw during our session on IO that there is an alternative, "sugared" syntax we can use when
--   performing computations in a context.

askForName :: IO ()
askForName = putStrLn "What is your name?"

nameStatement :: String -> String
nameStatement name = "Hello, " ++ name ++ "!"

helloName :: IO ()
helloName = askForName >> -- the value produced by this Action is `()`, so we discard it with `>>`
            getLine >>= -- the `IO String` value from the user is bound to the next function
            (pure . nameStatement) >>= -- this composite function takes a name `String`, produces a
                                       -- greeting `String` and lifts it to the IO context via `pure`
            putStrLn -- the `IO String` from the previous function is bound to `putStrLn` and printed

-- We can rewrite this IO Action using do-notation:

helloNameDo :: IO ()
helloNameDo = do
  askForName -- Computations performed using `>>` are written like single-line "statements".
             -- Note: these are still expressions - there are no statements in Haskell.
             -- But they allow us to write functional code in a familiar imperative style.
  name <- getLine -- Return values from computations that are in the context can be "unwrapped" with
                  --  `<-` to name the underlying data and process it.
  let greeting = nameStatement name -- We can optionally define intermediate variables using `let`.
      upperGreeting = map toUpper greeting
                                    -- This is useful for performing Calculations on the values we
                                    -- "unwrap" using the `<-` operator
  putStrLn upperGreeting -- The final line of a `do` block must be an expression returning a value of the
                           -- expected type (in this case `IO ()`, which is the return type of `putStrLn`).

-- Here is an example of rewriting the opposite way, from do-notation to `>>=`, `>>` and lambdas:
main :: IO ()
main = do
  name <- getLine
  let statement = nameStatement name
  putStrLn statement

main' :: IO ()
main' =
  getLine >>=
    (\name ->  -- the `<-` expands into `>>=` with a lambda expression, whose input is the return value
             --   of the previous monadic computation, but "unwrapped" from its context.
      (\statement -> -- the `let` keyword creates another lambda embedded in the previous one
        putStrLn statement) (nameStatement name))

-- While our first encounter with do-notation was in the IO context, it can be used when performing
--   computations in any monadic context.

-- We can also write generic monadic computations that can be reused across multiple contexts.
data Candidate = Candidate
  { candidateId :: Int
  , codeReview  :: Grade
  , cultureFit  :: Grade
  , education   :: Degree } deriving Show

data Grade  = F | D | C | B | A deriving (Eq, Ord, Enum, Show, Read)
data Degree = HS | BA | MS | PHD deriving (Eq, Ord, Enum, Show, Read)

viable :: Candidate -> Bool
viable c = all (== True) tests
  where
    passedCoding  = codeReview c == A
    passedCulture = cultureFit c >  C
    educationMin  = education c  >= MS
    tests = [ passedCoding
            , passedCulture
            , educationMin
            ]

assessCandidate :: Monad m => m Candidate -> m String
-- IO Candidate -> IO String
-- [Candidate] -> [String]
-- Maybe Candidate -> Maybe String
assessCandidate mC = do
  c <- mC
  let passed = viable c
      statement = if passed then "passed" else "failed"
  pure statement

exIO :: IO String
exIO = assessCandidate ioC
  where
    -- helper Action: receives String from user and converts to whichever type is needed
    readLine :: Read a => String -> IO a
    readLine field = putStrLn ("Enter a " ++ field ++ " value:") >> (read . map toUpper) <$> getLine
    ioC =
      -- use Applicative style to produce an `IO Candidate` value from user inputs:
      Candidate <$>
      readLine "ID" <*>
      readLine "Code Review" <*>
      readLine "Culture Fit" <*>
      readLine "Degree"

candidates :: [Candidate]
candidates = [ Candidate 1 A A BA
             , Candidate 2 C A PHD
             , Candidate 3 A B MS
             ]

exList :: [String]
exList = assessCandidate candidates

exMaybe :: Int -> Maybe String
exMaybe cId = assessCandidate $ M.lookup cId cMap
  where
    cMap = M.fromList $ zip [ 1.. ] candidates
-- Please see p. 209 - 212 in Get Programming with Haskell for explanation of the `Map` data type
