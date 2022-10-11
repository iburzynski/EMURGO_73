{-# LANGUAGE BangPatterns #-}

-- *** Notes on "Lazy" Evaluation ***

-- "In Haskell, the tree doesn’t fall in the woods until you walk through the forest and get to the
--    tree. For that matter, the tree didn’t exist until you walked up to it."

-- Non-strictness makes Haskell expressive:
--   * We can refer to values before we’ve done the work to create them.
--   * Evaluation is driven by *demand*, not by *construction*.
--   * Since it cannot be assumed in general that every subexpression will be needed, expressions
--       are evaluated from the *outside in*.

-- *** Evaluation Strategies ***

-- Call by Value:
--   * Argument expressions are evaluated before entering a function.
--   * Expressions that bindings reference are evaluated before creating the binding
--   * Inside-out evaluation
--   * Conventionally called *strict*

-- Call by Name:
--   * Expressions can be arguments to a function without being evaluated first
--   * Bindings can be created to expressions without evaluating them first
--   * Outside-in evaluation
--   * Non-strictness includes this strategy

-- Call by Need:
--   * Call by Name + Sharing
--   * Expressions are only evaluated once
--   * Only happens sometimes: when an expression isn't a lambda that takes arguments, and has a name
--   * Ensures termination whenever possible
--   * Never requires more steps than inside-out evaluation, and sometimes fewer
--   * "Lazy" means "delay and avoid" rather than just delay.

-- *** Sharing ***
-- When a computation is named, its result can be shared between all references to it without
--   re-evaluating it.
-- When a computation is in-lined (unnamed), its result isn't shared.
--   * Each inlined expression is treated as distinct, even if identical to another
-- Functions with named arguments don't share results
-- Functions in point-free notation (without named arguments) do

-- Typeclass constraints prevent sharing
--   * Under the hood, a typeclass constraint is technically a function awaiting an argument
--   * If you don’t declare a concrete type, GHC will have to re-evaluate your function every time,
--       because it can’t know that the type didn’t change somewhere along the way.
--       Because it remains a function and unapplied functions are not shareable values, polymorphic
--       expressions can’t be shared.
-- Values of a concrete, constant type can be shared, once evaluated.
-- Polymorphic values may be evaluated once but still not shared because, underneath, they continue
--   to be functions awaiting application.

-- GHC oscillates between call-by-need and call-by-name based on necessity and what it thinks will
--   produce faster code.
--   * It can do this without breaking your code because it knows when your code does or doesn't
--     have effects.

-- *** Weak Head Normal Form (WHNF) ***
-- Means an expression has been evaluated up to its "head", which means either:
--   * its outermost value constructor (i.e. `Just (1 + 2)`), or
--   * its outermost lambda expression (i.e. `\x -> x + (1 + 2)`)
-- The "head" here doesn't refer to the head of a list, but to the outermost function application
-- Reference: https://stackoverflow.com/questions/6872898/what-is-weak-head-normal-form

-- *** Thunks ***
-- Term for a suspended computation that *may be* performed at a later point
-- Not yet evaluated up to WHNF
-- GHC will not *thunk* (delay) values which it knows are constants: instead it evaluates
--   opportunistically.
-- GHC will stop evaluating as soon as it encounters some computation.

-- In some cases, it’s cheaper to just compute something than to create a thunk and evaluate later.
--   * Common in numeric code where you have a lot of Int and Double values which are individually
--     cheap to produce.

-- *** How to Impose Strictness ***

-- 1. Seq:
--  seq :: a -> b -> b
-- `seq` is a special function that forces evaluation of its first argument (up to WHNF)
--   * Can improve performance by avoiding unneeded laziness
--   * When we know a subexpression will need to be evaluated and want to avoid building up large
--       expressions in memory ("thunks"), we can force it to be evaluated ahead of time.

-- 2. Case/Pattern Matching
-- We also force evaluation when we case or pattern match on something
--   * Because matching requires a value to compare against patterns, evaluation must be performed
--   * Evaluation will only proceed up to the level of specificity required by the pattern, i.e.:
--       case x of
--          Just y -> ...
--          Nothing -> ...
--       Here the expression `x`, which produces some `Maybe` value, will be evaluated up to the
--         value constructor, to determine whether it is a `Just` or `Nothing` value.
--         Any unevaluated expression inside the `Just` constructor will not be evaluated, and will
--         simply bind to the variable `y`. To evaluate further, we'd have to destructure it further
--         in the case pattern.

-- 3. Bang Patterns
-- The `BangPatterns` language pragma allows opt-in strictness by prefixing values with a `!` symbol.
-- To use it, put the following at the very top of any .hs file: {-# LANGUAGE BangPatterns #-}

-- In this example, any expression passed to the parameter `b` will not be evaluated, since its
--   value isn't used in the function body:
doesntEval :: Bool -> Int
doesntEval b = 1

-- Calling `seq` on the value will force any expression passed to the parameter `b` to evaluate (up
--   to Weak Head Normal Form):
manualSeq :: Bool -> Int
manualSeq b = b `seq` 1

-- With `BangPatterns` enabled, prefixing `b` with `!` has the same effect as the `manualSeq`
--   example above:
banging :: Bool -> Int
banging !b = 1

-- `BangPatterns` also works in data declarations, making values passed to value constructors strict:
data Foo = Foo Int !Int

first (Foo x _) = x
second (Foo _ y) = y

-- This will not throw an error, because the first field of `Foo` is lazy by default, and its value
--   isn't being used:
ex1 = second $ Foo undefined 2

-- This will throw an error, because we are demanding the `undefined` value:
ex2 = first $ Foo undefined 2

-- This will also throw an error, as we'd expect:
ex3 = second $ Foo 1 undefined

-- But this will also throw an error, because we've made the second field of `Foo` strict using `!`:
ex4 = first $ Foo 1 undefined

-- 4. Strict Mode
-- The {-# LANGUAGE Strict #-} pragma will impose strict evaluation everywhere when it is enabled at
--   the top of the file.
-- Strict mode will not affect the behavior of code defined in other modules outside of the file,
--   which will remain lazy by default.
-- We can opt out of strict evaluation for particular values by prefixing them with `~`, which we
--   can think of as the inverse of the `!` prefix used with `BangPatterns`.
-- Can result in unexpected behavior, especially if we forget we are in Strict Mode!
-- Use if all or most of the code in a particular file should be evaluated strictly, for performance
--   reasons.


ex5 :: IO ()
ex5 = do
  let x :: Int
      x = undefined
  s <- getLine
  case s of
    "hi" -> print x
    _ -> putStrLn "hello"

-- In a strict language we couldn't evaluate `ex5` successfully at all
-- A strict language evaluates each binding as it comes into scope, not when it is used.
-- It would force the "bottom" value (`undefined`) before binding `x`.
-- In Haskell, this code will work fine unless the string "hi" is provided to the `getLine` action.

ex6 :: IO ()
ex6 = do
  let x :: Int
      x = undefined
  s <- getLine
  case s of
    "hi" -> print x
    _ -> x `seq` putStrLn "hello"

-- The version above will call `getLine` successfully, but then throws an error, because evaluation
--   of `x` is being forced with `seq` in the second case pattern.

ex7 :: IO ()
ex7 = do
  let x :: Int
      !x = undefined
  s <- getLine
  case s of
    "hi" -> print x
    _ -> putStrLn "hello"

-- The version above will throw an error immediately, because evaluation of `x` is being forced with
--   `!` at the point of assignment.


-- *** Additional Resources ***
-- *Haskell Programming from First Principles* Chapter 27 "Non-strictness"
-- Graham Hutton - "Lazy Evaluation": https://www.youtube.com/watch?v=R1uBhRK2AKI
-- Deep Dive: Takenobu Tani - "Lazy Evaluation Illustrated for Haskell Divers"
--    https://takenobu-hs.github.io/downloads/haskell_lazy_evaluation.pdf
