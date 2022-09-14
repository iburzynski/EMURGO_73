{-# LANGUAGE InstanceSigs #-}

import Control.Applicative

--- *** Maybe Applicative ***
-- We'll reimplement the built-in `Maybe` context and make it an Applicative Functor
data Maybe' a = Nothing' | Just' a
  deriving Show

-- Before we can define an Applicative instance, we need to make `Maybe'` a Functor
-- All Applicatives are also Functors: in other words, Functor is a "superclass" of Applicative
instance Functor Maybe' where
   fmap :: (a -> b) -> Maybe' a -> Maybe' b
   fmap _ Nothing'  = Nothing'
   fmap f (Just' x) = Just' (f x)

instance Applicative Maybe' where
   pure :: a -> Maybe' a
   pure = Just'

   (<*>) :: Maybe' (a -> b) -> Maybe' a -> Maybe' b
   Nothing' <*> _ = Nothing'
   _ <*> Nothing' = Nothing'
   Just' f <*> Just' x = Just' (f x)

sumMaybes = (+) <$> Just' 8 <*> Just' 7
--          Just' (\x -> 8 + x) <*> Just' 7 => Just' 15

sumOfThree x y z = x + y + z

sumMaybes' = sumOfThree <$> Just' 8 <*> Just' 7 <*> Just' 5
-- sumMaybes_ = pure sumOfThree <*> Just' 8 <*> Just' 7 <*> Just' 5