{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use fold" #-}
{-# LANGUAGE InstanceSigs #-}
module Monoids where

-- *** NOTE: ***
-- You can use the built-in types `Sum`, `Product`, `Min`, `Max`, etc. by importing `Data.Semigroup`:
-- import Data.Semigroup
-- For these exercises we will implement them ourselves.

-- *** Mixing Colors ***
-- We'll define a custom `Color` data type to explore the Semigroup and Monoid typeclasses:
data Color =
    Red
  | Yellow
  | Blue
  | Green
  | Purple
  | Orange
  | Brown
  deriving (Show, Eq)

-- *** Semigroup ***
-- The Semigroup typeclass provides a generic way to compose data of the same type

-- In math, a semigroup is defined as:
 -- "any set for which there is a binary operation that is closed and associative"
 -- closed: applying the binary operation on two values of the set yields another value of the set
   --In Haskell, this is expressed with the signature `a -> a -> a`

-- Semigroup has a single method, "mappend" (for "monoidal append"), expressed by the operator (<>)
  -- "mappend" is a binary operation taking two values of the same type and returning a new
  -- value of that type.
  -- (<>) :: Semigroup a => a -> a -> a

-- Semigroups must obey the *associativity* law:
  -- x <> (y <> z) == (x <> y) <> z
  -- This law isn't enforced by Haskell: it's up to us to uphold it by defining our type correctly

-- To make our `Color` type a Semigroup, we need to define how its values are combined:
instance Semigroup Color where
  (<>) :: Color -> Color -> Color
  Red <> Yellow = Orange
  Yellow <> Red = Orange
  Red <> Blue = Purple
  Blue <> Red = Purple
  Yellow <> Blue = Green
  Blue <> Yellow = Green
  x <> y
    | x == y = x
    | otherwise = Brown

ex1_1 = Red <> Yellow
ex1_2 = Red <> Blue
ex1_3 = Green <> Purple

-- Our type is "closed" under the (<>) operation.
-- But have we defined a valid Semigroup?
-- Let's see if `Color` satisfies the associativity law:
ex1_4 = (Green <> Blue) <> Yellow
ex1_5 = Green <> (Blue <> Yellow)

-- How can we fix our Semigroup instance to preserve associativity?

-- instance Semigroup Color where
--   (<>) :: Color -> Color -> Color
--   x <> y
--     | x == y = x
--     | all (`elem` [Red, Yellow, Orange]) [x, y] = Orange
--     | all (`elem` [Red, Blue, Purple]) [x, y] = Purple
--     | all (`elem` [Yellow, Blue, Green]) [x, y] = Green
--     | otherwise = Brown

-- *** Monoid ***
-- A Monoid is an augmented Semigroup:
  -- In addition to a "mappend" operation, it also has an *identity* element, called "mempty"
  -- Semigroup is a *superclass* of Monoid*
    -- * Note: the textbook says otherwise because it was published before this addition
-- The "mempty" value has a neutral effect when "mappended" to any other value of its type
  -- ex. `[]` is the `mempty` value for the List type
    -- [] <> [1, 2, 3] == [1, 2, 3]
    -- "hello" <> ""   == "hello"
  -- What should the `mempty` value for our Color type be?

-- data Color =
--     Red
--   | Yellow
--   | Blue
--   | Green
--   | Purple
--   | Orange
--   | Brown
--   | Clear <- mempty!
--   deriving (Show, Eq)

-- A Monoid must obey two *identity* laws:
  -- Left Identity:
  -- mempty <> x == x
  -- Right Identity:
  -- x <> mempty == x

-- class Semigroup a => Monoid a where
  -- * mempty  :: a * REQUIRED
  -- mappend :: a -> a -> a
  -- mconcat :: [a] -> a

-- The `mappend` method is a historical artifact from before Semigroup was introduced to Haskell
-- It is exactly the same as (<>), similar to how `return` is the same as `pure`.
-- It no longer needs to be implemented to define a Monoid instance.

-- `mconcat` is a monoidal fold operation:
  -- it takes a list of monoidal values and folds them into a single value
  -- definition: `mconcat = foldr (<>) mempty`
  -- `mconcat` doesn't need to be implemented manually:
  --  we gain this method automatically by just defining `mempty`

-- instance Monoid Color where
--   mempty = Clear

-- Exercise: create a list of Color values and see how they mix together using `mconcat`
-- colors :: [Color]
-- colors = [Yellow, Green, Yellow, Blue, Clear, Blue]

-- mixed :: Color
-- mixed = mconcat colors
-- equivalent to: foldr (<>) mempty [Yellow, Green, Yellow, Blue, Clear, Blue]

-- *** Multiple Instances: ***
-- A type can only have one Semigroup/Monoid definition
-- However, some types can form multiple semigroups/monoids under different operations
  -- Numeric types like Int/Float/etc. can form a semigroup/monoid under addition and multiplication
  -- In these cases, we don't define instances for the type itself
  -- Instead we use `newtype` wrappers to define new types and instantiate these:
newtype Sum a = Sum { getSum :: a } deriving Show
newtype Product a = Product { getProduct :: a } deriving Show

instance Num a => Semigroup (Sum a) where
  Sum x <> Sum y = Sum (x + y)

instance Num a => Semigroup (Product a) where
  Product x <> Product y = Product $ x * y

instance Num a => Monoid (Sum a) where
  mempty :: Sum a
  mempty = Sum 0

instance Num a => Monoid (Product a) where
  mempty = Product 1

foo = mconcat [Sum 1, Sum 32, Sum 41, Sum 17]
bar = mconcat [Product 12, Product 4, Product 27, Product 13]

-- *** Min & Max ***
-- Reference: "Silly Job Interview Questions in Haskell" https://chrispenner.ca/posts/interview

-- To further explore the utility of Monoids, we'll solve the following interview-style question:
-- Given a list of elements, find the smallest and largest element.

-- We can solve this easily with Haskell's built-in `minimum` and `maximum` functions:
simpleMinMax :: Ord a => [a] -> (a, a)
simpleMinMax xs = (minimum xs, maximum xs)

ex2_1 :: (Int, Int)
ex2_1 = simpleMinMax [3, 1, 10, 5]

-- But there's a problem with this solution: `minimum` and `maximum` are partial functions.
-- This makes our function partial as well:
ex2_2 :: (Int, Int)
ex2_2 = simpleMinMax []

-- `Min` and `Max` are `newtype` wrappers that can contain any Orderable data:
newtype Min a = Min { getMin :: a } deriving (Show, Eq, Ord, Bounded)
newtype Max a = Max { getMax :: a } deriving (Show, Eq, Ord, Bounded)

instance Ord a => Semigroup (Min a) where
  Min x <> Min y = Min $ min x y

instance Ord a => Semigroup (Max a) where
  Max x <> Max y = Max $ max x y

-- The Monoid instances require the wrapped data to have a `Bounded` instance,
-- and use the data's bounds to define the `mempty` values:
instance (Ord a, Bounded a) => Monoid (Min a) where
  mempty = maxBound

instance (Ord a, Bounded a) => Monoid (Max a) where
  mempty = minBound

ex2_3 :: Min Int
ex2_3 = Min 1 <> Min 2

ex2_4 :: Max Int
ex2_4 = Max 1 <> Max 2

ex2_5 :: Min Int
ex2_5 = mempty

ex2_6 :: Max Int
ex2_6 = mempty

ex2_7 :: Min Int
ex2_7 = mconcat [Min 1, Min 2, Min 3]

ex2_8 :: Max Int
ex2_8 = mconcat [Max 1, Max 2, Max 3]

-- Using the power of monoids, we can define a better min/max function that isn't partial:
boundedMinMax :: (Bounded a, Ord a) => [a] -> (a, a)
boundedMinMax xs = (n, m)
  where
    (Min n, Max m) = mconcat $ map aToMinMax xs
    aToMinMax x = (Min x, Max x)
    -- `aToMinMax` takes a value of type `a` and creates a tuple of type `(Min a, Max a)`
    --  A 2-tuple is also an instance of `Semigroup` (if its elements are also Semigroups).
      -- Here's how its instance is defined:
      -- instance (Semigroup a, Semigroup b) => Semigroup (a, b) where
      --   (x, y) <> (x', y') = (x <> x', y <> y') -- elements are `mappend`ed to make a new tuple
    -- When we `mconcat` a list of these tuples, we get a single tuple with the min and max values
    --  for the entire list.

    -- map aToMinMax [1, 2, 3, 4] => [(Min 1, Max 1), (Min 2, Max 2), (Min 3, Max 3), (Min 4, Max 4)]
    -- foldr (<>) (Min 9223372036854775807, Max -9223372036854775808) [(Min 1, Max 1), (Min 2, Max 2), (Min 3, Max 3), (Min 4, Max 4)]
    -- (Min 1, Max 4)

-- * Note: mapping a value into a Monoid followed by `mconcat` (here `mconcat $ map aToMinMax`)
--   has a built-in function called `foldMap` :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
--   We've used a more verbose map/mconcat version in these examples for clarity, but could also do:
  -- `foldMap aToMinMax xs`

ex2_9 :: (Int, Int)
ex2_9 = boundedMinMax [4, 1, 23, 7]

ex2_10 :: (Int, Int)
ex2_10 = boundedMinMax []
-- This still isn't quite what we'd like.
-- It's much better for our function not to fail when applied to an empty list...
-- But returning the `maxBound` and `minBound` values doesn't really make sense either.

-- We can make an even better implementation using another Monoid, which we've worked with before:
data Maybe' a = Nothing' | Just' a
  deriving Show
-- *Note: here we are reimplementing the built-in `Maybe` type to illustrate its monoidal properties.

instance Semigroup a => Semigroup (Maybe' a) where
  Just' x  <> Just' y  = Just' $ x <> y
  Nothing' <> Just' y  = Just' y -- a null "mappended" to a non-null returns the non-null value
  Just' x  <> Nothing' = Just' x

instance Semigroup a => Monoid (Maybe' a) where
  mempty = Nothing' -- the identity value for the Maybe Monoid is the null value
  mconcat = foldr (<>) Nothing' -- implemented manually here for illustration only (not required)

-- What happens if we embed our Min/Max tuple inside a `Maybe` context?
-- Our mapping function just wraps the tuple with the `Just` constructor:
-- How will this solve our empty list problem?
aToMaybeMinMax :: Ord a => a -> Maybe (Min a, Max a)
aToMaybeMinMax x = Just (Min x, Max x)

ex2_11 :: Ord a => Maybe (Min a, Max a)
ex2_11 = mconcat $ map aToMaybeMinMax []
-- Because of how `mempty` is defined for Maybe, and the definition of `mconcat`,
-- folding over an empty list now returns a sensible value (`Nothing`).

-- Our final implementation:
minMax :: Ord a => [a] -> Maybe (a, a)
minMax xs = case mconcat $ map aToMaybeMinMax xs of
  Just (Min x, Max y) -> Just (x, y) -- remove the `Min` and `Max` wrappers
  _ -> Nothing

ex2_12 :: Maybe (Int, Int)
ex2_12 = minMax [4, 1, 9, 5]

ex2_13 :: Maybe (Int, Int)
ex2_13 = minMax []

-- Homework exercise:
-- Define Semigroup and Monoid instances for our custom `List` type that we created previously.

-- Here is the type declaration and `Show` instance from before:
data List a = Empty | Cons a (List a)

instance Show a => Show (List a) where
  show :: Show a => List a -> String
  show xs = showList True xs
    where
      showList :: Show a => Bool -> List a -> String
      showList True   Empty         = "[]"
      showList True  (Cons x Empty) = "[" ++ show x ++ "]"
      showList True  (Cons x xs)    = "[" ++ show x ++ "," ++ showList False xs
      showList False (Cons x Empty) = show x ++ "]"
      showList False (Cons x xs)    = show x ++ "," ++ showList False xs

instance Semigroup (List a) where
  (<>) :: List a -> List a -> List a
  -- Hint 1: you'll need to write 3 function patterns
  -- Hint 2: you'll need to use recursion to `mappend` two non-empty lists
  -- Hint 3: don't destructure the lists unless you need to!
  (<>) = undefined

instance Monoid (List a) where
  mempty :: List a
  mempty = undefined

-- If you've defined your instances correctly, you'll be able to run the following in GHCi to concat
-- two custom lists together.
ex3_1 = mconcat [ Cons 'g' (Cons 'o' (Cons 'o' (Cons 'd' (Cons ' ' Empty))))
                , Cons 'j' (Cons 'o' (Cons 'b' (Cons '!' Empty)))]

-- Further reading/practice:
-- *Get Programming with Haskell* Lesson 17: "Design by Composition: Semigroups and Monoids"
-- Complete the probability tables example beginning on pg. 196
-- Complete Lesson 20 Capstone: Time Series*
  -- * Note: you'll need to read Section 18.2.3 on Data.Map (pg. 209 - 212) to complete the capstone
  --   Map isn't covered in our curriculum but is a very useful data structure to learn