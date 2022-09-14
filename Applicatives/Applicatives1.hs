{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}

-- *** Limitations of the Functor Typeclass ***
-- The Functor typeclass's method, `fmap` (<$>), only lets us apply unary (single-parameter)
--  functions to a single value in a context.

maybeVal = Just "reverse me"
reversedMaybeVal = fmap reverse maybeVal -- reverse <$> maybeVal

listVals = ["reverse me", "palindrome", "tacocat"]
reversedListVals = reverse <$> listVals

ioVal = getLine
reversedIoVal = reverse <$> ioVal

-- What if we need to apply a binary function to two arguments which are both in a context?
maybeVal1 = Just "a monad is just a monoid "
maybeVal2 = Just "in the category of endofunctors"

-- We can't use `fmap` twice like this:
-- appendMaybes = (++) <$> maybeVal1 <$> maybeVal2
-- To understand why not, let's break down what's happening:

  -- (++) :: [a] ->  [a] -> [a]
  -- or with explicit currying...
  -- (++) :: [a] -> ([a] -> [a])
  --                  ^ the return value of our `fmapped` function will be a new function `[a] -> [a]`
  -- fmap :: (a -> b) -> f  a   -> f   b
  -- fmap    (++) ::     f [a]  -> f ([a] -> [a])
  --                 Just "str" -> Just (\s -> "str" ++ s)
  -- Our append function, (++), is promoted into a Functor context (like `Maybe`).
  -- It is now waiting for some argument `f [a]` (a list of `a` values, in the Functor context)

  -- When we apply `maybeVal1` to our promoted function, what's the signature of the returned value?
  -- λ > :t (++) <$> maybeVal1
  -- λ > Maybe ([Char] -> [Char])

  -- We can think of our result so far like this:
  -- Just (\s -> "a monad is just a monoid " ++ s)

  -- `fmap` won't work to apply this function to a second argument...
  --  Our partially-applied function is now inside the `Maybe` context and doesn't satisfy the type:
  --   fmap :: (a -> b) -> f a -> f b
  --           ^ `fmap` must receive an ordinary function, not a function in a context!

-- How can we solve this type mismatch without resorting to repetitive DIY functions again?
-- Don't worry - there's an "app" for that!
-- (<*>) :: f (a -> b) -> f a -> f b
-- Note the similarity to the ($) operator (ordinary function application):
-- ($)   ::   (a -> b) ->   a ->   b
-- This is why we call these functors "applicatives": they support application within a context (`f`)

-- Ordinary function application:
--      `g      x      y      z` <- "g applied to x, y and z"
-- Applicative style (in some applicative context `f`):
-- `pure g <*> fx <*> fy <*> fz` <- here `x`, `y`, and `z` are all values in the context `f`
-- The syntax is the same, except we need to lift `g` into the context (with `pure`),
--   then use `<*>` as adapters between each of the (contextualized) arguments

-- (<*>) ::     f     (a      -> b)      -> f     a      -> f     b
--              Maybe ([Char] -> [Char]) -> Maybe [Char] -> Maybe [Char]
appendMaybes =  pure  (++)              <*> maybeVal1   <*> maybeVal2


-- Note: `f <$> x` == `pure f <*> x`. We could write our function above like this instead:
appendMaybes' = ((++) <$> maybeVal1) <*> maybeVal2 -- extra parens added for clarity
-- We can use either syntax interchangeably. The version using `fmap` is more concise.

-- Recap:
-- Any time we use `fmap` to partially apply a function to some argument in a context, we receive a
--  new function in that context. We need a special "adapter" to continue applying it to arguments.

-- The Applicative "app" operator (<*>) allows us to chain together any number of arguments in a
--   context, which means we can sequence an arbitrary number of computations using existing
--   functions instead of creating redundant custom functions.

-- An Applicative Functor isn't very different from an ordinary Functor with respect to the
--  underlying problem it helps us solve: eliminating redundancy by applying existing functions to
--  data inside any suitable context. Our "superfunctor" just broadens the scope of which simple
--  functions we can do this with (functions with 2+ input parameters).

minOfThree :: (Ord a) => a -> a -> a -> a
minOfThree x y z = min x $ min y z -- same as min x (min y z), but let's practice using ($)
-- minOfThree x y z = minimum [x, y, z]

readInt :: IO Int
readInt = read <$> getLine -- `read` is promoted to the IO context and applied to the String inside

minOfThreeIO :: IO Int
minOfThreeIO = minOfThree <$> readInt <*> readInt <*> readInt
               -- IO (Int -> Int -> Int)
                                      -- IO (Int -> Int)
                                                     -- IO Int

-- We can keep using `<*>` to apply as many arguments as we need. Here is a function with 4 inputs:
minOfFour :: (Ord a) => a -> a -> a -> a -> a
minOfFour w x y z = min w . min x $ min y z
-- minOfFour w x y z = minimum [w, x, y, z]

minOfFourIO :: IO Int
minOfFourIO = pure minOfFour <*> readInt <*> readInt <*> readInt <*> readInt

-- *** Using Applicatives to Construct Values in a Context ***
-- Value constructors are just functions, so we can use `<*>` to create values of any type using
--  context-bound data:
data FullName = FullName { getFirst  :: String
                         , getMiddle :: String
                         , getLast   :: String
                         } deriving Show

-- We can construct values using record syntax, providing the values in any order like this:
-- myname = FullName { getFirst = "Ian", getLast = "Burzynski", getMiddle = "T."}

-- But even though we've defined our type using record syntax, we can still construct values
--  without referencing the field names, like we would with a simple product type
--  (as long as the field values are passed to the value constructor in the correct order):
myName = FullName "Ian" "T." "Burzynski"
myName' = FullName {getLast = "Burzynski", getMiddle = "T.", getFirst = "Ian"}

-- Using this simple construction syntax, we can use `<*>` to apply our constructor to IO strings:
fullNameIO :: IO FullName
fullNameIO = FullName  <$> getLine <*> getLine <*> getLine
--           ^ constructor ^ getFirst  ^ getMiddle ^ getLast
-- Alternate syntax using pure/app:
--           pure FullName <*> getLine <*> getLine <*> getLine

-- Or `Maybe` strings:
fullNameMaybe :: Maybe FullName
fullNameMaybe = FullName <$> Just "Ian" <*> Just "T." <*> Just "Burzynski"
fullNameMaybe' = FullName <$> Just "Ian" <*> Nothing <*> Just "Burzynski"