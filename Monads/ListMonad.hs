{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use list comprehension" #-}
{-# LANGUAGE InstanceSigs #-}
import Data.List (transpose)

----------------------------------------------------------------------------------------------------
--                                  *** The List Monad ***
----------------------------------------------------------------------------------------------------
-- Reference:
--   * https://en.m.wikibooks.org/wiki/Haskell/Understanding_monads/List
--   * Get Programming with Haskell by Will Kurt, Lesson 32: The List Monad and List Comprehensions

-- As monads, lists are used to model nondeterministic computations, which may return an arbitrary
--   number of results.
-- Like how the Maybe context represents computations which could return zero or one value,
--   with the List context we can return zero, one, or many, depending on the length of the list.

----------------------------------------------------------------------------------------------------
--                 *** PART I: Implementing a List Monad from Scratch ***
----------------------------------------------------------------------------------------------------
-- To fully understand the List Monad, we'll pretend it doesn't exist and build it ourselves.

-- First we'll define our own List type, which parallels the built-in one:
data List a = Empty | Cons a   (List a) deriving Show
--   []   a = []    |      a : ([]   a) <- comparisons to the built-in list type are provided below

-- Now we'll define a Functor instance for our List context, so we can map over its values:
instance Functor List where
  fmap :: (a -> b) -> List a -> List b
  fmap _ Empty        = Empty
  fmap ab (Cons a as) = Cons (ab a) (fmap ab as)
  --                         ^ create a new head, applying the function to the old one
  --                                ^ repeat the process over the tail

-- We'll need an equivalent to the built-in `++` (append) operation for our List values (it's needed
--   to define an Applicative instance, as we'll see soon).
-- We can gain this functionality via a Semigroup instance, with `<>` (mappend) behaving like `++`:
instance Semigroup (List a) where
  xs          <> Empty = xs -- mappending an empty value leaves the other unchanged
  -- xs       ++ []    = xs
  Empty       <> ys    = ys
  -- []       ++ ys    = ys
  (Cons x xs) <> ys    = Cons x   (xs <> ys)
  -- (x:xs)   ++ ys    =      x : (xs ++ ys)
  --                     ^ make a new List
  --                          ^ leave the head the same
  --                              ^ mappend the second list to the tail of the first, recursively

-- Defining an Applicative instance for our Lists allows us to apply Lists of functions to lists of
--   arguments. The result is a Cartesian Product: every possible pairing of values from the two.
instance Applicative List where
  pure :: a -> List a
  pure a = Cons a Empty -- `pure` takes a value and promotes it to a singleton List
  -- pure a = a : []

  (<*>) :: List (a -> b) -> List a -> List b
  Cons f fs <*> xs = (f <$> xs) <> (fs <*> xs)
  -- (f:fs) <*> xs = (map f xs) ++ (fs <*> xs)
  --   ^ destructure the List of functions to get the head and tail
  --                  ^ map the head function, applying it to every value of the argument list `xs`
  --                               ^ repeat this process recursively with all remaining functions...
  --                             ^ appending each to the results of the previous map operations

-- We're almost ready to make List into a monadic context, but we're lacking one necessary function.
-- As we know, Monads are defined by the "bind" operation (>>=), which has this general signature:
--   (>>=) :: Monad m => m a -> (a -> m b) -> m b
-- In the case of lists, its signature is:
--                       [a] -> (a -> [b])    -> [b]
-- and in our DIY List context, it will be:
--                    List a -> (a -> List b) -> List b

-- *** How Bind Works with Lists: ***
-- Bind will pull the values out of a List, passing them to a function producing a new List.
-- We first map a function `(a -> List b)` over a `List a`, getting a List of Lists: List (List b).
-- Note that this is the only way we can approach our desired return type of `List b`:
--    * The only values we have to work with have types `List a` and `(a -> List b)`.
--    * We can't directly apply a function `(a -> List b)` to a value `List a`: we have to map it.
--    * Mapping a function with a return type of `List b` on a List produces a List of Lists of `b`.
-- We now have a redundant layer of List context, preventing our bind function from type checking.
-- We need some function that flattens that List of Lists into a List.
--    * For the built-in list type, this function exists and is called `concat` (:: [[a]] -> [a]).
--    * There's even a handy composite function `concatMap`, which performs a map and then flattens.
--    * The bind implementation for lists can therefore be defined as `xs >>= f = concatMap f xs` .
--    * However, since we're building our own List type, we don't have access to concat/concatMap.
--    * No problem: we'll make our own `concatMap` by defining Monoid and Foldable instances.

-- The "Foldable" class includes types that act as containers for values, which we can fold/reduce
--   into some summary value. We've worked with folds for the built-in list type, but there are
--   other built-in Foldable types, and we can also make our own types instances of Foldable.
instance Foldable List where -- Note: Foldable instances require a kind signature `* -> *`, similar
                             --  to Functor, Applicative and Monad. This means we define the
                             --  instance for the type constructor `List` (:k * -> *), not a
                             --  finished type like `List a` (:k *).
  -- To instantiate a Foldable, we can implement either `foldr` or `foldMap`. `foldMap` will end up
  --   serving as our substitute for `concatMap`, but `foldr` is simpler to implement, and by doing
  --   so we'll gain `foldMap` for free (along with `foldl` and a host of other methods). Very cool!
  foldr :: (a -> b -> b) -> b -> List a -> b
  foldr    _ z Empty       = z
  -- foldr _ z []          = z
  foldr    f z (Cons x xs) = f x (foldr f z xs)
  -- foldr f z (     x:xs) = f x (foldr f z xs)

  -- Hopefully this implementation looks familiar! We take a binary ("reducer") function `f`,
  --   an initial value `z`, and a list of values, and recursively fold them until we reach the base
  --   case (`Empty`). The resulting expression is simplified down into a single value of type `b`.

-- We're almost ready to use `foldMap` to map a function (a -> List a) over a List of `a`'s, and
--   flatten the resulting nested list. But when we check the type signature for `foldMap` we'll see
--   there is one requirement we haven't satisfied:

-- foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m)
-- Description from Haskell docs: Map each element of the structure into a monoid, and combine the
--   results with (<>).

-- We want to use `foldMap` with a signature like this:
-- foldMap :: (a -> List b) -> List a ->  List b
--  where `List` substitutes for `t` and `List b` for `m`.

-- Our `List` context is a `Foldable`, so it can substitute for `t`.
-- But we haven't made it a Monoid yet - only a Semigroup - so we can append values together using
--   (<>), but we haven't defined an identity (or "unit") element, which is required for `foldMap`.

-- No problem: defining a Monoid instance is a breeze!
instance Monoid (List a) where
  mempty = Empty -- extending our Semigroup to a Monoid is simple: we just define the identity value

-- We can now use `foldMap` with our List type, without needing to implement it ourselves.
-- But if we were to peek under the hood to see how it works, it would look like this:
--  foldMap f xs = foldr (\x z -> f x <> z) mempty xs

-- When we use it on our List, it maps the function `f` (a -> List b) to each element (which results
--   in a List), then appends that List to an accumulator (also a List).
-- The result of this is identical to the behavior of `concatMap` on built-in lists.

-- It's also identical to how (>>=) should behave, so we're ready to complete our journey:
instance Monad List where
  (>>=) :: List a -> (a -> List b) -> List b
  xs >>= f = foldMap f xs

-- instance Monad [] where
--   (>>=) :: [a] -> (a -> [b]) -> [b]
--   xs >>= f = concatMap f xs

-- Phew! That was a lot of work, and not something we'll ever need to do when writing Haskell.
-- Thankfully all of this functionality is available for us with the built-in list type, but now we
--   understand how lists really work, and why we can think of them as a monadic context instead of
--   merely a data structure that stores multiple values.

-- Let's recap what we just accomplished:
--  1. built a viable substitute for lists by defining a recursive sum type
--  2. unlocked the same functionality as `map` for lists by making List a Functor
--  3. unlocked the same functionality as `++` by making List a Semigroup
--  4. unlocked the ability to perform non-deterministic computations by making List an Applicative
--  5. unlocked the ability to fold over List data by making it a Foldable
--  6. extended the mapping capability of List to include the functionality of `concat`, by making it
--       a Monoid and using `foldMap`
--  7. as a consequence of #6, unlocked the ability to chain together sequences of computations that
--       return Lists (a -> List b), by making List a Monad

-- Now to better understand the utility of #7, we'll examine a practical example of monadic
--   computations in the context of lists, once again using the game of Tic-Tac-Toe.

----------------------------------------------------------------------------------------------------
--          *** PART II: List-Tac-Toe: Non-deterministic Computation in Action ***
----------------------------------------------------------------------------------------------------

-- A practical example of non-deterministic computation is developing a turn-based game, in which we
--   may wish to find all possible board states.

-- We'll show how this can be achieved with the List Monad for a game of Tic-Tac-Toe.
-- Note that we're using the built-in list type, not our "List" type from Part I, which we built
--   from first principles strictly for illustrative purposes.

data Tick = X | O | E deriving (Show, Eq) -- define our types
type Row = [ Tick ]
type Board = [ Row ]

-- We need a function that takes a Tick value (X or O) representing the active player, and a board
--  state, and calculates a list of possible new board states that result from the player making
--  each possible move.
makeBoards :: Tick -> Board -> [Board]
makeBoards t b
  | gameOver b = [ ] -- if the board is complete, no further boards can be produced from it
-- For a non-empty board, we produce every possible next board that the active player could make.
-- We can do this in one of three ways
-- Syntax 1. Using bind and lambdas (this is what Syntax 2 & 3 "desugar to"):
  | otherwise = [ 0 .. 2 ] >>= -- take initial range 0 - 2, corresponding to possible row indices
  -- (>>=) maps the following lambda function to each value and flattens the results:
    (\i -> [ 0 .. 2 ] >>= -- a second range 0 - 2, corresponding to possible column indices
      -- (>>=) maps the following lambda function to each value and flattens the results:
      (\j -> if isEmpty b i j
               then [ fillPos t b i j ] -- fillPos makes a new Board with requested position filled
               else [ ]))
    -- How does the execution happen?
    -- Layer I: a list is produced from mapping the 1st lambda expression to
    --   the 1st range [0, 1, 2], containing 3 results.
    --   I[0]: `i` is bound to 0, yielding the following expression:
    --      ([ 0 .. 2] >>=
    --        (\j -> if isEmpty b 0 j
    --                  then [ fillPos t b 0 j ] -- Q: Why do we need to wrap this in a singleton?
    --                  else [ ]))
    --      Layer II: a new, nested list is produced by mapping the 2nd lambda to the 2nd range
    --        [0, 1, 2], containing 3 results. We'll call these II[0], II[1] and II[2].
    --      II[0]: `j` is bound to 0, yielding the following expression:
    --          (if isEmpty b 0 0
    --            Layer III: the final return value is embedded in an additional layer of List:
    --            then [ fillPos t b 0 0 ]) -- III[0]: either a singleton list (one Board)...
    --            else [ ] -- ...or an empty list (no Boards).
    --      II[1]: `j` is bound to 1... produces III[1]
    --      II[2]: `j` is bound to 2... produces III[2]
    --   I[1]: `i` is bound to 1...
    --      II[0]: `j` is bound to 0... produces III[0]
    --      II[1]: `j` is bound to 1... produces III[1]
    --      II[2]: `j` is bound to 2... produces III[2]
    --   I[2]: `i` is bound to 2...
    --      II[0]: `j` is bound to 0... produces III[0]
    --      II[1]: `j` is bound to 1... produces III[1]
    --      II[2]: `j` is bound to 2... produces III[2]

    --  If no flattening were to take place along the way, we'd now have data of type [[[Board]]].
    --    * An example value would look like this:
    --      [ <- Layer I
    --        [ <- Layer II
    --          [a0] <- Layer III
    --        , [a1]
    --        , [a2]
    --        ]
    --      , [ [b0]
    --        , [b1]
    --        , [b2]
    --        ]
    --      , [ [c0]
    --        , [c1]
    --        , [c2]
    --        ]
    --      ]
    --      where [a0] .. [c2] are singleton lists at Layer III returned by the inner lambda
    --        (these could also be empty lists, if their corresponding position was already filled)
    --    * The list at Layer II is the result of mapping the inner lambda to the second range
    --    * The list at Layer I  is the result of mapping the outer lambda to the first range
    --    * Note how this structure mimics the layout of our tic-tac-toe board:
    --        * Layer I corresponds to a list of rows, created by the first range
    --        * Layer II corresponds to a list of columns, created by the second range
    --        * Instead of containing Ticks, Layer II contains singleton lists of Board states in
    --            which the corresponding row/column position was filled by the active player.

    -- However, the definition of (>>=) for lists doesn't just map the lambda onto a list: it also
    --  flattens it afterward using `concat`.
    -- Since we are using (>>=) twice, this means two layers of list will be flattened away, and
    --   instead of a return value of type [[[Board]]], we get a value of our desired type [Board].
    -- Put differently, Layer III becomes Layer I after the two binds are complete.
    -- If the return value of the inner lambda wasn't wrapped in an extra Layer of context,
    --   not only would the lambda not satisfy the return type requirement of bind (here [Board]),
    --   we'd no longer have a list of Boards after the two flattening operations (the 2nd `concat`
    --   would try to flatten too far and compress our list of Boards into a single Board!

    -- The final result when called on an empty board is a list of 9 Board values.
    --    Binding this result to another call to `makeBoards` will repeat this process on each Board
    --      from the first result, yielding 9 x 8 possible boards.
    --    The number of possible Board states grows dramatically, but starts to slow as Boards begin
    --      producing completed Boards (either by a player winning or a tie).

-- Syntax 2. do-notation (this is syntactic sugar for the bind/lambda version above):
  -- | otherwise = do
  --   i <- [ 0 .. 2] -- Take initial range 0 - 2, corresponding to possible row indices
  --                     The range is bound to a nested lambda expression with input parameter `i`.
  --   j <- [ 0 .. 2] -- A second range 0 - 2, corresponding to possible column indices, is bound to
  --                     a nested lambda expression with input parameter `j`.
  --   if isEmpty b i j -- for each possible pairing of `i` and `j`, we check for an empty location
  --     then [ fillIndex t b i j ] -- if empty, we make a singleton list containing a new Board
  --     else [ ] -- if not, we return an empty list
  --

-- Syntax 3. list comprehension (this "sugars" the do-notation version even further):
--  | otherwise = [ fillIndex t b i j | i <- [0 .. 2], j <- [0 .. 2], isEmpty b i j]
--                  ^ The result of the comprehension comes first:
--                     * Here you can combine data and apply functions however you like.
--                     * We achieve the same functionality as mapping here by just applying functions

--                [ fillIndex t b i j | i <- [0 .. 2], j <- [0 .. 2], isEmpty b i j]
--                                    ^ The pipe symbol separates the result from the generator(s)
--                                      and predicate(s).

--                [ fillIndex t b i j | i <- [0 .. 2], j <- [0 .. 2], isEmpty b i j]
--                                      ^ Generators come next and behave just like in do-notation,
--                                          but are separated by commas instead of newlines.
--                                          Generators are multiplicative: think of them like nested
--                                          loops.

--                [ fillIndex t b i j | i <- [0 .. 2], j <- [0 .. 2], isEmpty b i j]
--                                                                    ^ Predicates can be added to
--                                                                      filter out undesired results

-- *** Testing the Monadic Behavior ***
-- Note: the code used in this tutorial is far from optimized, production-grade Haskell.
--   It's inefficient and resource-intensive to run, so be careful running `testLengths` or
--     `allTurns`, depending on your system specs. Running `allTurns` will also print an enormous stream of text to the console,
--      and should be understandable without actually running.

-- Create an empty 3 x 3 board to start:
initBoard :: Board
initBoard = replicate 3 (replicate 3 E)
--- [[E, E, E], [E, E, E], [E, E, E]]

makeBoards' :: Tick -> Board -> [Board]
makeBoards' t b
  | gameOver b = [ ]
  | otherwise = [ fillPos t b i j | i <- [0 .. 2], j <- [0 .. 2], isEmpty b i j ]

turn1 :: [Board]
turn1 = makeBoards X initBoard -- Player X takes the first turn, producing a list of Boards.

-- We can continue binding the result of each call to a new one to perform multiple turns:
turn2 = turn1 >>= makeBoards O
turn3 = turn2 >>= makeBoards X
turn4 = turn3 >>= makeBoards O
turn5 = turn4 >>= makeBoards X
turn6 = turn5 >>= makeBoards O
turn7 = turn6 >>= makeBoards X
turn8 = turn7 >>= makeBoards O

-- See how many possible Board states there are after each of the first 8 turns:
testLengths :: [Int]
testLengths = map length [turn1, turn2, turn3, turn4, turn5, turn6, turn7, turn8]

-- We can also sequence multiple turns easily using a `do` block. The following behaves the same as
--  the separate turns above, but is combined into a single expression.
--  Do-notation for lists is difficult to understand at first...
allTurns :: [Board]
allTurns = do
  turn1 <- makeBoards X initBoard -- It appears we are converting our list of Boards into a single
                                  --   Board element, which is confusing.
  -- Instead, we should think of it like this:
  --  Each time we bind a result of `makeBoards` to a variable, we're creating a "generator" that
  --  behaves similar to an imperative loop. Each subsequent generator is nested within the previous
  --  one, just like a nested loop. The total possible Boards generated will be the product of all
  --  the possibilities generated at each level.
  turn2   <- makeBoards O turn1 -- Extra whitespace has been added to illustrate the nesting
  turn3     <- makeBoards X turn2
  turn4       <- makeBoards O turn3
  turn5         <- makeBoards X turn4
  turn6           <- makeBoards O turn5
  turn7             <- makeBoards X turn6
  makeBoards O turn7 -- We don't bind the final turn to anything: just run it and return its value.

-- And finally, with a list comprehension:
allTurnsComprehension :: [Board]
allTurnsComprehension =
  [ turn8 | turn1 <- makeBoards X initBoard
  -- ^ we could optionally transform the return value here in some way by applying a function to it
          , turn2 <- makeBoards O turn1
          , turn3 <- makeBoards X turn2
          , turn4 <- makeBoards O turn3
          , turn5 <- makeBoards X turn4
          , turn6 <- makeBoards O turn5
          , turn7 <- makeBoards X turn6
          , turn8 <- makeBoards O turn7
  -- we could optionally add one or more predicate conditions here to filter out certain results
  ]

----------------------------------------END-OF-LESSON-----------------------------------------------

-- *** Helper Functions: don't worry about the details unless you're interested! ***

-- Helpers to check if a position is empty and fill it with a Tick:
isEmpty :: Board -> Int -> Int -> Bool
isEmpty b i j = b !! i !! j == E

fillPos :: Tick -> Board -> Int -> Int -> Board
fillPos t b i j = go b i j
  where
    go (r:rs) 0 j = fillJ r j : rs
    go (r:rs) i j = r : go rs (i - 1) j
    fillJ (_:cs) 0 = t : cs
    fillJ (c:cs) j = c : fillJ cs (j - 1)

-- Helpers to determine if a board is complete (`makeBoards` will produce empty lists if so):
gameOver :: Board -> Bool
gameOver b = any ($ b) [ tied
                       , horizMate
                       , vertMate
                       , diagMate
                       ]
  where
    tied :: Board -> Bool
    tied = all (notElem E)

horizMate :: Board -> Bool
horizMate []           = False
horizMate (row : rows) = all (== X) row ||
                         all (== O) row ||
                         horizMate rows

vertMate :: Board -> Bool
vertMate rows = horizMate (transpose rows)

diagMate :: Board -> Bool
diagMate rows = all (== X) (trace rows) ||
                all (== O) (trace rows) ||
                all (== X) (antitrace rows) ||
                all (== O) (antitrace rows)
  where
    trace :: Board -> Row
    trace []           = []
    trace (row : rows) = head row : trace (map tail rows)
    antitrace :: Board -> Row
    antitrace []           = []
    antitrace (row : rows) = last row : antitrace (map init rows)