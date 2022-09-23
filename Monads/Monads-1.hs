{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Replace case with maybe" #-}
{-# HLINT ignore "Use <&>" #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# LANGUAGE InstanceSigs #-}
{-# HLINT ignore "Use <$>" #-}

import qualified Data.Map as Map

-- Problem: we want to chain together multiple computations, each of which takes some simple value
--  for input, but returns a value in some context `m`.

-- Example 1: we have two values of type `Float`, `x` and `y`.
--  * We want to divide `x` by `y` and take the square root of the quotient: `sqrt (x / y)`
--  * These are "unsafe" operations that may produce invalid outputs:
--    * Dividing by zero returns `Infinity`
--    * Taking the square root of a negative number returns `NaN`
--    * We would like to handle these cases more elegantly, using the `Maybe` context and its `Nothing` value.

safeDiv :: Float -> Float -> Maybe Float
safeDiv x 0 = Nothing
safeDiv x y = Just $ x / y

safeSqrt :: Float -> Maybe Float
safeSqrt x
  | x < 0 = Nothing
  | otherwise = Just $ sqrt x

-- As usual, we can write a custom function to handle the composition:
safeDivSqrt :: Float -> Float -> Maybe Float
safeDivSqrt x y = case safeDiv x y of
  Nothing -> Nothing
  Just z  -> safeSqrt z


-- Example 2:
type UserName = String
type GamerId = Int
type PlayerCredits = Int

userNameDB :: Map.Map GamerId UserName
userNameDB = Map.fromList [ (1, "nYarlanthoTep")
                          , (2, "KINGinYELLOW")
                          , (3, "dagon1997")
                          , (4, "rcarter1919")
                          , (5, "xCTHULHUx")
                          , (6, "yogSOThoth")
                          ]
-- the `fromList` function takes a list of key/value pairs and converts them into a `Map` (dictionary/lookup table-like data structure)

creditsDB :: Map.Map UserName PlayerCredits
creditsDB = Map.fromList [ ("nYarlanthoTep", 2000)
                         , ("KINGinYELLOW", 15000)
                         , ("dagon1997", 300)
                         , ("rcarter1919", 12)
                         , ("xCTHULHUx", 50000)
                         , ("yogSOThoth", 150000)
                         ]

-- Problem: we want a function that looks up a user's credits given their GamerId.
--   * This function should return a value in the `Maybe` context, in case the Id or an associated entry in `creditsDB` is missing.
--   * It is essentially composed of two helper lookup functions:
lookupUserName :: GamerId -> Maybe UserName
lookupUserName i = Map.lookup i userNameDB

lookupCredits :: UserName -> Maybe PlayerCredits
lookupCredits u = Map.lookup u creditsDB
-- The signature of their composition, after applying `lookupUserName` to the `GamerId` argument:

-- Maybe UserName -> (UserName -> Maybe PlayerCredits) -> Maybe PlayerCredits
-- Expressing the general pattern in a more polymorphic way:
--  m a -> (a -> m b) -> m b

-- We have no way to compose the computations together using the adapters currently at our disposal (<$>, <*>, pure):
-- If we `fmap` the function `(a -> m b)` to `m a`, we will get a value of type `m (m b)` instead of `m b`.
user6 :: Maybe UserName
user6 = lookupUserName 6

credits6 :: Maybe (Maybe PlayerCredits)
credits6 = lookupCredits <$> user6

-- Same with lifting the function using `pure` and then applying it via `<*>`.
-- We end up with a redundant layer of context that needs to be flattened somehow.

-- We can fall back on a suboptimal solution, writing an alternate version of our second computation:
lookupCredits' :: Maybe UserName -> Maybe PlayerCredits
lookupCredits' Nothing  = Nothing
lookupCredits' (Just u) = lookupCredits u

-- and then compose it with `lookupUserName` like so:
creditsFromId :: GamerId -> Maybe PlayerCredits
creditsFromId = lookupCredits' . lookupUserName

-- In both examples, we've solved our current problem, but the solutions are suboptimal for the usual reasons:
--  * They are highly specific, and thus not reusable/scalable.
--    * We'll need to craft similar DIY functions every time we need to compose a different set of computations within this context
--    * Each of these bespoke functions will only be useful in the context of `Maybe`, and can't be ported to other contexts (List, Either, IO, etc.)
--  * Our DIY solution is straightforward due to the simplicity of the `Maybe` context and our ability to easily pattern match on its value constructors.
--    * Other contexts are more cumbersome to work with, and in the IO context we can't pattern match at all, so a DIY approach isn't even possible.

-- *** Monads & the Bind Adapter ***
-- As with previous scenarios we've encountered, we want to eschew these brittle, specific solutions in favor of abstract adapters.
-- We need a "super applicative" class with an additional method, allowing us to overcome composition challenges arising from *any* combination of computations, occurring within *any* context.
-- This "super applicative" is called a Monad, and this method is called "bind" (>>=).
-- (>>=) :: Monad m => m a -> (a -> m b) -> m b

-- We encountered this operator already when we began to work with IO Actions.
-- Since IO values cannot escape their IO context, we had to use this operator (or its cousin,
-- "sequence" (>>)) in order to compose even simple sequences of Actions (i.e. `getLine >>= putStrLn`)
-- We can use the same operation to chain together computations in any context with a Monad instance, not just IO.
-- This allows us to solve the problems we encountered earlier with `Maybe`:

safeDivSqrt' :: Float -> Float -> Maybe Float
safeDivSqrt' x y = safeDiv x y >>= safeSqrt

creditsFromId' :: GamerId -> Maybe PlayerCredits
creditsFromId' i = lookupUserName i >>= lookupCredits

-- These monadic solutions are endlessly scalable: we can continue binding computations onto the chain without any manual intervention required!
-- This is the essence of what a Monad is in programming, and why Monads are ubiquitous in Haskell.

-- *** Exercise: Implement the Monad instance for the `Maybe` context ***

data Maybe' a = Nothing' | Just' a

instance Functor Maybe' where
  fmap _ Nothing'  = Nothing'
  fmap f (Just' x) = Just' $ f x

instance Applicative Maybe' where
  pure = Just'

  Nothing' <*> _ = Nothing'
  Just' f <*> mx = f <$> mx

instance Monad Maybe' where
  (>>=) :: Maybe' a -> (a -> Maybe' b) -> Maybe' b
  Nothing' >>= _ = Nothing'
  Just' x  >>= f = f x