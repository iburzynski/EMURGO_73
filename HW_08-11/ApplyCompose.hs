{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Redundant bracket" #-}
module ApplyCompose where

-- *** Apply ***
apply :: (a -> b) -> a -> b
-- apply f x = f x
f `apply` x = f x

-- Example:
-- :: ( a  ->  b )   ->         a           ->             b
--             ^ the function can return a different type than its input, but doesn't have to.
--
--    ([a] -> [a])   ->        [a]          ->            [a]
--   filter (> 10) `apply` map (^2) [1..10] == [16, 25, 36, 49, 64, 81, 100]
--                         ^ the entire expression on the right is passed as the 2nd arg. to `apply`
--                           its return type must match the input type of the function on the left
--   ^ `apply` then applies the function on the left to that value on the right

-- This is how the ($) operator works. We can replace `apply` with `$` in our example:
foo :: [Int]
foo = filter (> 10) $ map (^2) [1..10]

-- Rules for Apply:
-- The left argument to ($) must be a single-parameter (unary) *function*.
-- The right argument must be a *value* matching the type of that function's input.


-- *** Compose ***
-- Compose takes two functions with compatible input/output
-- It returns a new function that applies them both
compose :: (b -> c) -> (a -> b) -> (a -> c)
compose g f = (\a -> g (f a))
--             ^ returns a new function awaiting one argument
--               the function applies `f` to that argument and then applies `g` to the result

-- Example:
-- :: (b   ->    c)     ->    (a  -> b) -> (a  -> c)
--    filter (> 10) `compose` map (^ 2) == \xs -> filter (>10) (map (^ 2) xs)

-- This is how the (.) operator works. We can replace `compose` with `.` in our example:
bar :: [Int] -> [Int]
bar = filter (> 10) . map (^2)
-- Note how compose allows us to easily assemble composite functions out of smaller ones
-- We can name these composite functions and use them elsewhere for more concise, readable code.

-- Rules for Compose:
-- The left and right arguments passed to (.) must both be *functions*.
-- The return type of the right function must match the input type of the left function.
-- `bar` above is valid because `map (^2)` returns [Int], and `filter (> 10)` takes [Int].

-- Often (.) and ($) are used together in the same expression:
baz = map show . filter (> 10) $ map (^2) [1..10]
--             ^ note how there is a function on both sides of the compose operator
--                             ^ and with apply, a function on the left, a value on the right
--                               (calling `map (^2)` on the list of 1 to 10 results in a value)
-- This will produce the same result:
blah :: [String]
    -- (c -> d) -> (b -> c) -> (a -> b) -> a -> d
blah = map show . filter (> 10) . map (^ 2) $ [1..10]
--              ^               ^
--              we can chain as many functions as we want using multiple compose operators

-- This is not valid syntax:
-- map show . filter (> 10) . map (^ 2) [1..10]
--                          ^ compose expects a function on the right, but we're passing a value

-- This syntax is valid, but not idiomatic Haskell:
fooo  = map show $ filter (> 10) $ map (^ 2) [1..10]
-- We can see why it works if we replace the apply operators with parentheses:
foooo = map show  (filter (> 10)  (map (^ 2) [1..10]))
-- It's possible to use application in place of composition...
-- but Haskellers prefer building composite functions and then applying them to a single input value.

-- This syntax is not valid:
-- map show . filter (> 10) . map (^ 2) ([1..10])
-- This is due to precedence: regular function application has precedence over infix operators
-- So GHC tries to treat ([1..10]) as an argument to `map (^2)` rather than the composite function
-- `map (^ 2) ([1..10])` has type [Int], but the compose operator to its left expects a function

myFunc = map show . filter (> 10) . map (^2)

-- But this syntax is valid:
blahBlah = (map show . filter (> 10) . map (^ 2)) [1..10]
--          ^ because the composite function has type [Int] -> [String]
--                                                 ^ and [1..10] has type [Int]

