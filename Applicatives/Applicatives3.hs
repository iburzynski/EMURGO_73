{-# LANGUAGE InstanceSigs #-}

import Control.Applicative hiding (ZipList)

-- *** Exercise: Implement the Applicative Instance for List ***
-- Now we will create our own version of the built-in List type from scratch and make it an Applicative:
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

-- Built-in List equivalents:
-- Empty == []
-- Cons 1 (Cons 2 (Cons 3 Empty)) == [1, 2, 3]

-- We will need some version of `++` (append) for our List type in our Applicative instance.
-- Recall that we implemented this by making our List a Semigroup, defining `<>` ("mappend"):
instance Semigroup (List a) where
  (<>) :: List a -> List a -> List a
  Empty <> xs = xs
  -- Built-in List equivalent:
  -- [] <> xs = xs
  xs <> Empty = xs
  Cons x xs <> ys = Cons x (xs <> ys)
  -- Built-in List equivalent:
  -- (x:xs) ++ ys = x : xs ++ ys
  --                x : x' : x'' ... : y : y' : y'' ... : []

instance Functor List where
  fmap :: (a -> b) -> List a -> List b
  fmap _ Empty = Empty
  -- fmap _ [] = []
  fmap f (x `Cons` xs) = f x `Cons` fmap f xs
  -- fmap f (x : xs) = f x : fmap f xs

data Either' e a = Left' e | Right' a

instance Functor (Either' e) where
  fmap :: (a -> b) -> Either' e a -> Either' e b
  fmap f (Right' x) = Right' $ f x
  fmap _ (Left' e)  = Left' e

instance Applicative (Either' e) where
  (<*>) :: Either' e (a -> b) -> Either' e a -> Either' e b
  Left' e <*> _ = Left' e
  Right' f <*> ex = f <$> ex
  -- _ <*> Left' e = Left' e
--  Right' f <*> Right' x = Right' (f x)

  pure :: a -> Either' e a
  pure = Right'

instance Applicative List where
  pure :: a -> List a
  pure x = x `Cons` Empty
  -- Built-in List equivalent:
  -- pure x = [x]

  (<*>) :: List (a -> b) -> List a -> List b
  Empty <*> _        = Empty
  f `Cons` fs <*> xs = (f <$> xs) <> (fs <*> xs)
  --  Step 1: map the first function to all values in the arguments list
  --  Step 2: append that mapped list to the result of recursively `app`ing the tail of the
  --    functions list to the arguments list
  -- Example: [(+ 1), (* 2), (^ 2)] <*> [1, 2, 3]
  --  (+ 1) <$> [1, 2, 3] => [2, 3, 4] ++
  --                                      (* 2) <$> [1, 2, 3] => [2, 4, 6] ++
  --                                                                          (^2) <$> [1, 2, 3] => [1, 4, 9] ++ []

combos = Cons (+ 1) (Cons (* 2) (Cons (^ 2) Empty)) <*> Cons 1 (Cons 2 (Cons 3 Empty))
-- i.e. [(+ 1), (* 2), (^ 2)] <*> [1, 2, 3]

-- Exercise Q29.3 (Get Programming with Haskell)
-- You bought soda last night but don't remember whether it was a 6-pack or 12-pack:
startingSoda :: [Int]
startingSoda = [6, 12]

-- You and your roommate each drank 2 sodas yesterday:
remainingSoda :: [Int]
-- remainingSoda = (\sodas -> sodas - 4) <$> startingSoda
remainingSoda = subtract 4 <$> startingSoda

-- You're having 2 or 3 friends come over:
guests :: [Int]
guests = [2, 3]

-- The total number of people (guests + you + roommate):
totalPeople :: [Int]
totalPeople = (+ 2) <$> guests

-- Each person will drink 3 or 4 sodas:
sodasPerPerson :: [Int]
sodasPerPerson = [3, 4]

-- Calculate how many sodas are needed in total:
sodasNeeded :: [Int]
sodasNeeded = (*) <$> sodasPerPerson <*> totalPeople

-- Calculate how many you need to buy:
sodasToBuy :: [Int]
sodasToBuy = (-) <$> sodasNeeded <*> remainingSoda