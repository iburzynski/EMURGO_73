
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}
{-# HLINT ignore "Avoid lambda" #-}

-- Let's declare a "top-level" variable with a constant value
x = 1
-- We can now refer to x anywhere in our program, as an alias for the value 1

-- We declare a function the same way, but with one or more "local" variables declared as parameters.
-- Function parameters are separated by spaces.
-- Parameter names are arbitrary. We can call them whatever we like!
-- Here we declare a function `simple`, with a single parameter `y`.
simple y = y
-- What will happen if we try to return the value of `y` in GHCI?

-- Variables used as parameters in functions aren't accessible from the top-level scope!
-- They only exist inside the function's scope.

xPlusY y = x + y
-- what will `xPlusY 2` evaluate to?

-- We can refer to variables from an outer scope inside of our function, without defining them as parameters. Our function doesn't violate the transparency principle, despite seeming to have an implicit input, because variables in Haskell are just names for constant values.

xPlusY' x y = x + y
-- * We use a quotation mark after names in Haskell to declare an alternate version of something, to avoid a naming conflict.

-- what will `xPlusY' 2 2` evaluate to?

overwrite x =
  let x = 2
  in
    let x = 3
    in
      x

x' = overwrite x
-- what is the value of x'?
-- what is the value of x?

myLambda1 = \x -> x + 1
-- how would we call myLambda1 to get a result of 5?

myLambda2 = \x y -> x + y
-- give an example call to myLambda2