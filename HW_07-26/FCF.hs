module FCF where

-- We'll need to use some built-in list functions that aren't included in Haskell's Prelude.
-- We can import other Base modules like this:
import Data.List
-- Imports must be declared above the rest of your code.


-- *** Using First-Class Functions for Code Reuse ***

-- Let's say we have several arithmetic functions we want to apply, but only when the input is even.
-- Without first-class functions, we'd have to define them as multiple functions:
-- ifEvenInc, ifEvenDouble, ifEvenSquare, etc.
-- and each of them would repeat the same logic of checking if its input is even.

-- This violates a central programming maxim called the D.R.Y. Principle ("Don't Repeat Yourself")
-- We can avoid code redundancy by abstracting the even-checking into its own function `ifEven`...
-- Since functions are first-class, we can pass our arithmetic function (`inc`, `double`, `square`)
-- as an argument to `ifEven` and apply it to the numeric input when it's even:

-- Arithmetic functions
inc n    = n + 1
double n = n * 2
square n = n ^ 2

-- Higher-order function, receiving arithmetic function as an argument (`myFunction`):
ifEven myFunction x = if even x
                      then myFunction x
                      else x

-- Now we can easily define specialized versions of `ifEven` without redundant code:
ifEvenInc n    = ifEven inc n
ifEvenDouble n = ifEven double n
ifEvenSquare n = ifEven square n

-- Since arithmetic functions are so simple, we can also spare ourselves some code by using lambdas.
-- Instead of defining them as separate named functions, we use in-line anonymous functions:
ifEvenCube n = ifEven (\x -> x ^ 3) n
-- This is the main use case for lambdas: they make using higher-order functions more ergonomic.
-- However, lambdas are more difficult to reason about than named functions, which are
-- self-documenting and thus reduce cognitive load. It's best to use them only for simple functions.

-- If you need to make use of smaller helper functions inside your bigger function, you can always
-- use a `where` block and create named functions inside.
ifEvenFoo n = ifEven incDoubleSquare n
  where
    incDoubleSquare x = ((x + 1) * 2) ^ 2

-- *** Custom Sorting with Higher-Order Functions ***
-- A common use of higher-order functions is to provide custom sorting logic to a sort function.

-- *** Tuples ***
-- A data structure to hold a fixed quantity of different values, which can be of different types
fullName  = ("Simon", "Peyton", "Jones")
employee = (42, "Simon")
-- A tuple with two elements is called a *pair*.
-- Pairs come with built-in functions `fst` and `snd` to access their elements.

haskellers = [
    ("Simon", "Peyton Jones")
  , ("Philip", "Wadler")
  , ("Paul", "Hudak")
  , ("Michael", "Peyton Jones")
  ]

-- *** Ordering && `sortBy` ***
-- Haskell has a special data type called Ordering with three values:
-- Greater than: GT, Less than: LT, Equal: EQ
-- A custom sorting function is a function that compares two arguments and returns an Ordering value
-- The built-in `sortBy` function takes a sorting function and a list as its arguments:
haskellers' = sortBy compareLastNames haskellers

-- Let's define a function to sort the list of Haskellers by last name and then first name
compareLastNames name1 name2
  | lastName1  > lastName2  = GT
  | lastName1  < lastName2  = LT
-- if last names are the same, compare first names
  | firstName1 > firstName2 = GT
  | firstName1 < firstName2 = LT
  | otherwise               = EQ
  where
    -- we define some local variables in a `where` block to prevent repetition
    lastName1  = snd name1
    lastName2  = snd name2
    firstName1 = fst name1
    firstName2 = fst name2

-- Here is the same function defined using a case expression.
-- We can use the built-in `compare` function rather than comparing manually:
-- compare 1 2 == LT
-- compare 2 1 == GT
-- compare 1 1 == EQ

compareLastNames' name1 name2 =
  case compare lastName1 lastName2 of
-- if last names are different, we just return the Ordering:
  GT -> GT
  LT -> LT
-- if last names are the same, compare first names
  EQ -> compare firstName1 firstName2
  where
-- we can use destructuring to get the tuple elements without calling `fst` and `snd`:
    (firstName1, lastName1) = name1
    (firstName2, lastName2) = name2

-- *** Returning Functions as Output ***

-- Let's start with a generic `title` function
-- It takes a title string and a (<First>, <Last>) name tuple and returns "<Title> <Last>"
title t name  = t ++ " " ++ snd name
-- Now we can specialize this function to make more specific versions:
titleDr name = title "Dr." name
titleF  name = title "Ms." name
titleM  name = title "Mr." name

-- We'll define a list of people with data in the following format:
-- ((<First>, <Last>), <Gender>, <Doctor>)
-- Notice we can nest a tuple inside of another tuple
people =
  [
    (("Theodor", "Seuss"),   'm', True)
  , (("Kolleen", "Jackson"), 'f', False)
  , (("Mr", "Bean"),         'm', False)
  ]

-- Now let's make a "routing" function: it takes a person and returns the appropriate title function
getTitleFunc person =
  if doctor
  then titleDr
  else case gender of
    'f' -> titleF
    'm' -> titleM
  where
-- use destructuring to get person's gender/doctor values, using wildcard '_' for unused name value
    (_, gender, doctor) = person

-- Our final function takes a person, selects the appropriate title function and applies it:
addTitle person = titleFunc name
  where
    titleFunc    = getTitleFunc person
    (name, _, _) = person
-- Now we can map this function over a list of people to transform them into titled names:
people' = map addTitle people

-- This example follows the same logic, but shows us an interesting behavior of Haskell:
seuss  = (("Theodor", "Seuss"), 'm', True)
seuss' = let (name, _, _) = seuss
         in getTitleFunc seuss name
-- 1. `getTitleFunc` is applied to the first argument `seuss`, returning the function `titleDr`
-- 2. `titleDr` is then applied to the next argument `name`
-- We can think of this as: `(getTitleFunc seuss) name`, but the parentheses aren't needed
-- In the next topic we'll see how this behavior allows us to partially apply arguments to functions