{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}

-- Use function patterns to implement the following list functions:

-- Takes any list and returns a string describing its size:
-- "empty", "singleton", "two element list", "three or more element list"
describeList :: [a] -> [Char] -- could also be String
describeList [_]     = "singleton"
describeList [_, _]  = "two elements"
describeList (_ : _) = "three or more element list"
describeList []      = "empty"

-- implement the built-in `head` function:
myHead :: [a] -> a
myHead (x : _) = x
myHead [] = error "empty list"

--implement the built-in `tail` function:
myTail :: [a] -> [a]
myTail (_ : xs) = xs
myTail [] = error "empty list"

-- Homework: implement the following built-in functions to the best of your ability. The name of the
--   built-in function is mentioned above each example between backticks. Use the search feature at
--   http://hoogle.haskell.org to look up the documentation for the corresponding functions and try
--   using them in GHCi to understand how they work before implementing your version.

--- Use function patterns and recursion to define the following functions:

-- `last`:
myLast :: [a] -> a
myLast [x] = x
myLast (_ : xs) = myLast xs
myLast [] = error "empty list"

ex = myLast ([1 .. ] :: [Integer])

-- `init`:
myInit :: [a] -> [a]
myInit [_] = []
myInit (x : xs) = x : myInit xs
myInit [] = error "empty list"

-- `length`:
myLength :: [a] -> Int
myLength [] = 0
myLength (_ : xs) = 1 + myLength xs

-- `reverse`:
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x : xs) = myReverse xs ++ [x] -- (x : [])

-- `elem`:
myElem :: Eq a => a -> [a] -> Bool
myElem q (x : xs) = q == x || myElem q xs
myElem _ [] = False

-- `(!!)`:
myIndex :: [a] -> Int -> a
myIndex _ i | i < 0 = error "negative index"
myIndex []  _      = error "index too large"
myIndex (x : _)  0 = x
myIndex (_ : xs) i = myIndex xs (i - 1)

-- For the next two functions, use a conditional to make sure the integer argument is > 0
-- What will happen if we don't do this?

-- `take`:
myTake :: Int -> [a] -> [a]
myTake n _ | n < 1 = []
myTake _ [] = []
myTake n (x : xs) = x : myTake (n - 1) xs

-- `drop`:
myDrop :: Int -> [a] -> [a]
myDrop n xs | n < 1 = xs
myDrop n (_ : xs) = myDrop (n - 1) xs
myDrop _ [] = []

-- `sum`:
mySum :: Num a => [a] -> a
mySum [] = 0
mySum (x : xs) = x + mySum xs

foo :: (Show a, Num a, Show b, Num b) => [a] -> [b] -> [String]
foo (x : xs) (y : ys) = (show x ++ show y) : foo xs ys

-- `zip`:
myZip :: [a] -> [b] -> [(a, b)]
myZip (x : xs) (y : ys) = (x, y) : myZip xs ys
myZip _ _ = []