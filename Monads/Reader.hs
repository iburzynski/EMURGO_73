{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use id" #-}
import Data.Char ( toUpper )

-- *** Part 2 - the Reader Monad: simulating a read-only global variable (constant) ***
-- Reference: https://williamyaoh.com/posts/2020-07-19-deriving-reader-monad.html

-- We want our program to have some kind of global runtime config:
--  * i.e. using different database connection strings depending on deployment environment
-- We could use the State monad, but would prefer the config be read-only (set once at runtime)

-- Example: for some reason we're doing an A/B test where we exclude the letter E or L
-- We define ABConfig as a product type with two Bool fields indicating whether each test is active:
data ABConfig = ABConfig
  { noEs :: Bool
  , noLs :: Bool
  }

-- We could add the config as an extra parameter to all our functions and pass it downward:
toUpperStr :: ABConfig -> String -> String
-- convert the string to uppercase and keep only characters that pass all the filter predicates
toUpperStr cfg str = filter passesAll (map toUpper str)
  where
    -- check if a character satisfies all the predicates in a list
    passesAll :: Char -> Bool
    passesAll char = all (\pred -> pred char) preds
    -- ex. on 'E' with `noEs` test enabled:
    -- passesAll 'E' => all [('E' /= 'E'), ('E' /= 'L')] => all [False, True] => False

    -- create a list of predicate functions to filter with:
    preds :: [Char -> Bool]
    preds =
      [ if noEs cfg then (/= 'E') else const True
    --                                 ^ the predicate always returns True if the test is disabled
      , if noLs cfg then (/= 'L') else const True
      ]

-- Any function that calls `toUpperStr` needs to pass the ABConfig value downward:
welcomeMessage :: ABConfig -> String -> String -> String
welcomeMessage cfg msg username = concat [
    "Welcome, "
  , toUpperStr cfg username
  , "! Message of the day: "
  , toUpperStr cfg msg
  ]

type FirstName = String
type NickName = String
type LastName = String

fullName :: ABConfig -> FirstName -> NickName -> LastName -> String
fullName cfg fn nn ln = concat [
    toUpperStr cfg fn
  , " \""
  , toUpperStr cfg nn
  , "\" "
  , toUpperStr cfg ln
  ]

-- This solution is annoying: we need to manually pass our "global" through every single function
-- Many of them may do nothing with the ABConfig data themselves...
-- but still need it as a parameter so they can pass it on to functions that do!
-- We're also increasing surface area for possible errors:
--   * i.e. mixing up parameter order if the "global" has the same type as another parameter

-- The core of a configurable function is one that takes in a config and produces a value.
-- We can abstract this into its own type, which we'll call Reader:
newtype Reader c a = Reader { runReader :: c -> a }
-- If we squint a little, we'll notice this type is essentially just a generic function (a -> b)

-- If we make it a monad, we can abstract away the config variable into the monadic context
-- This way we can chain configurable functions together using (>>=) and ignore the config parameter

toUpperStrM :: String -> Reader ABConfig String
toUpperStrM str = Reader (\cfg ->
  let
    passesAll char = all (\pred -> pred char) preds
    preds =
      [ if noEs cfg then (/= 'E') else const True
      , if noLs cfg then (/= 'L') else const True
      ]
  in
    filter passesAll (map toUpper str))
-- Same function as before, but the ABConfig parameter has been moved into the Reader value

welcomeMessageM :: String -> String -> Reader ABConfig String
welcomeMessageM msg username =
  -- Now we can pass the ABConfig implicitly via (>>=), only working with the Strings in the Readers
  toUpperStrM msg >>= (\upperMsg ->
    toUpperStrM username >>= (\upperUsername ->
      Reader (\_ ->
      --       ^ we don't do anything with the config value, so we can use a wildcard placeholder
                    concat [ "Welcome, "
                           , upperUsername
                           , "! Message of the day: "
                           , upperMsg
                           ])))

fullNameM :: FirstName -> NickName -> LastName -> Reader ABConfig String
fullNameM fn nn ln =
  toUpperStrM fn >>= (\upperFN ->
    toUpperStrM nn >>= (\upperNN ->
      toUpperStrM ln >>= (\upperLN ->
        Reader (\_ -> concat [ upperFN
                             , " \""
                             , upperNN
                             , "\" "
                             , upperLN
                             ]))))

instance Functor (Reader g) where
  fmap :: (a -> b) -> Reader c a -> Reader c b
  fmap ab (Reader ca) = Reader (\c -> ab $ ca c)
  --                    same as: Reader (ab . ca)

instance Applicative (Reader c) where
  pure :: a -> Reader c a
  pure a = Reader (\c -> a)
  --       same as: Reader $ const a

  (<*>) :: Reader c (a -> b) -> Reader c a -> Reader c b
  Reader cab <*> Reader ca = Reader (\c -> cab c (ca c))

instance Monad (Reader g) where
  (>>=) :: Reader c a -> (a -> Reader c b) -> Reader c b
  Reader ca >>= aRcb = Reader (\c -> runReader (aRcb (ca c)) c)

-- Since we abstracted over the config parameter, we no longer have a way to access it
-- (it has become part of the monadic context)
-- But some functions like `toUpperStr` need to inspect the config

-- We can solve this by implementing a utility function to get the config value:
ask :: Reader c c
ask = Reader (\c -> c)
--    same as: Reader id
-- `ask` duplicates the config value from Reader's first type parameter to its second type parameter
-- This way it can be accessed outside the monadic context

-- We can also define a utility function to transform the config:
asks :: (c -> a) -> Reader c a
asks ca = Reader (\c -> ca c)
--        same as: Reader ca

-- With these utility functions, we can refactor our functions again using do-notation:
toUpperStr' :: String -> Reader ABConfig String
toUpperStr' str = do
  cfg <- ask -- get the config value from the Reader context
  let
    preds =
      [ if noEs cfg then (/= 'E') else const True
      , if noLs cfg then (/= 'L') else const True
      ]
    passesAll c = all (\pred -> pred c) preds
  pure . filter passesAll . map toUpper $ str

welcomeMessage' :: String -> String -> Reader ABConfig String
welcomeMessage' msg username = do
  upperMsg      <- toUpperStr' msg
  upperUsername <- toUpperStr' username
  pure $ concat [ "Welcome, "
                , upperUsername
                , "! Message of the day: "
                , upperMsg
                ]

fullName' :: FirstName -> NickName -> LastName -> Reader ABConfig String
fullName' fn nn ln = do
  upperFN <- toUpperStr' fn
  upperNN <- toUpperStr' nn
  upperLN <- toUpperStr' ln
  pure $ concat [ upperFN
                , " \""
                , upperNN
                , "\" "
                , upperLN
                ]

-- Bonus: we can run a sub-function as if it is using a different config, like a "local environment"
-- The environment reverts once we return to the current function.
local :: (c -> l) -> Reader l a -> Reader c a
--       ^ function that transforms global config to a local config
--                   ^ a Reader that uses the local config
--                                 ^ returns a Reader with the original config restored
local cl (Reader la) = Reader (\c -> la (cl c))
--                       same as: Reader (la . cl)