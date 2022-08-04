{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use map" #-}
{-# HLINT ignore "Use or" #-}
{-# HLINT ignore "Use sum" #-}
{-# HLINT ignore "Use product" #-}
{-# HLINT ignore "Use foldr" #-}

import Data.List (foldl')

----------------------------------------------------------------------------------------------------
--                          *** HOMEWORK: Higher-Order Functions ***
----------------------------------------------------------------------------------------------------

-- *** Part I: Map/Filter ***
-- Implement your own versions of the `map` and `filter` functions using primitive recursion.

myMap :: (a -> b) -> [a] -> [b]
myMap = undefined

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter = undefined

-- *** Part II: Folds ***
-- Implement the following built-in functions using folds.
-- For each solution, think carefully about the following:
--  1. Which type of fold should be used?
--      * Use `foldr` whenever possible
--      * If a left fold is needed, use the strict version `foldl'` (imported from Data.List module)
--  2. Which binary function should be used for the reducer?
--      * Is there a built-in operator or function you can use?
--      * If not, write your own using a lambda expression
--      * Be mindful of parameter order!
--  3. What is the initial (identity) value?

mySum :: Num a => [a] -> a
mySum = undefined

myProduct :: Num a => [a] -> a
myProduct = undefined

-- For the following boolean functions, remember the built-in logical operators (||) and (&&):
-- and [True, False, True]   == False
-- and [True, True, True]    == True
-- or  [True, False, True]   == True
-- or  [False, False, False] == False

-- Fold Arguments
-- 1. Binary function (reducer)
-- 2. Initial (identity) value
-- 3. Collection (foldable)

myAnd :: [Bool] -> Bool
-- https://www.stackage.org/haddock/lts-19.6/base-4.15.1.0/Prelude.html#v:and
myAnd = undefined

myOr :: [Bool] -> Bool
-- https://www.stackage.org/haddock/lts-19.6/base-4.15.1.0/Prelude.html#v:or
myOr = undefined

myElem :: Eq a => a -> [a] -> Bool
myElem = undefined

-- The next 3 functions take a predicate (function returning a Bool) as their first parameter

myAny :: (a -> Bool) -> [a] -> Bool
-- https://www.stackage.org/haddock/lts-19.6/base-4.15.1.0/Prelude.html#v:any
myAny = undefined

myAll :: (a -> Bool) -> [a] -> Bool
-- https://www.stackage.org/haddock/lts-19.6/base-4.15.1.0/Prelude.html#v:all
myAll = undefined

myFilter' :: (a -> Bool) -> [a] -> [a]
myFilter' = undefined

myMap' :: (a -> b) -> [a] -> [b]
myMap' = undefined

-- Implement `reverse` using `foldr`.
myReverseR :: [a] -> [a]
myReverseR = undefined

-- Implement `reverse` using `foldl'`.
-- For a challenge, you can try using the `flip` function: flip :: (a -> b -> c) -> (b -> a -> c)
-- https://www.stackage.org/haddock/lts-19.6/base-4.15.1.0/Prelude.html#v:flip
myReverseL :: [a] -> [a]
myReverseL = undefined

-- The next two functions are partial functions: they return an error when called on an empty list.
-- Implement the non-empty patterns using folds:
-- Hint: for the initial/identity value, use the head of the list and fold over the tail.

myMaximum :: Ord a => [a] -> a
--https://www.stackage.org/haddock/lts-19.6/base-4.15.1.0/Prelude.html#v:maximum
myMaximum [] = error "empty list"
myMaximum (x:xs) = undefined

myMinimum :: Ord a => [a] -> a
--https://www.stackage.org/haddock/lts-19.6/base-4.15.1.0/Prelude.html#v:minimum
myMinimum [] = error "empty list"
myMinimum (x:xs) = undefined