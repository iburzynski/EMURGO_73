-- We define our pure functions ("Calculations") in a separate library module:
module Lib where

type Correct = Bool

checkGuess :: Int -> Int -> (String, Correct)
checkGuess s g = case compare g s of
  EQ -> ("You win!", True)
  LT -> ("Too low - guess again!", False)
  GT -> ("Too high - guess again!", False)

-- We'll import this code into our Main module and call it inside our IO Actions.