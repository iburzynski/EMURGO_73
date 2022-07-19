-- Note: you will need to save the FCF.hs file in the same directory as this file to load it in GHCi
import FCF ( inc, double, square, ifEven )

-- *** Function Closures ***
-- Closures allow us to dynamically create new functions

-- Let's make a function to generate new "ifEven" functions:
genIfEven f = (\x -> ifEven f x)
-- This could also be viewed as a nested lambda function:
-- genIfEven = (\f -> (\x -> ifEven f x))
-- Let's walk through how this function behaves: we provide `genIfEven` a function argument `f`...
-- and receive a new function, which takes an argument `x`, and applies `ifEven` to `f` and `x`.

-- Here the `f` argument is "captured" inside the lambda function's backpack (closure)
-- In technical terms, we say the lambda "closes over" f
-- Our baby lambda now takes on a life of its own; its origin story from `genIfEven` is lost
-- However, it retains `f` inside its backpack forever: its parent's scope is carried on

-- Now we can supply any function to `genIfEven` to create a specialized "ifEven" function:
ifEvenInc = genIfEven inc
-- Let's look at the nested lambda version again to understand how the application works:
--          (\f -> (\x -> ifEven f x)) inc
--                                    `inc` binds to `f`, which gives us:
--          (\x -> ifEven inc x)    -- our new function!

-- We could also make a version that captures the number (`x`)
genIfXEven x = (\f -> ifEven f x)
-- As a nested lambda:
-- genIfXEven = (\x -> (\f -> ifEven f x))
-- Here we capture the `x` argument inside the lambda's backpack (the lambda "closes over" x)

-- *** Partial Application ***
-- In Haskell we rarely need to define closures explicitly using lambdas like above.
-- We can simply call a function with fewer arguments than its total parameters
-- The function will then return a new function that's waiting for the remaining arguments.

-- `ifEven` has two parameters: a function `f` and a number `x`
ifEvenInc' = ifEven inc
-- In our new version, we provide `ifEven` only `inc` as an argument (which binds to parameter `f`)
-- The result is a function with one parameter `x`, just like our lambda above: (\x -> ifEven inc x)

ifEvenDouble = ifEven double
ifEvenSquare = ifEven square

-- *** Greeter Example:
-- If we define a general greeting function that joins a greeting string with a name string:
greeter greeting name = greeting ++ ", " ++ name ++ "!"

-- we can make specialized greeting functions by partially applying `greeter` to various greetings:
hello = greeter "Hello"
hola = greeter "Hola"
bonjour = greeter "Bonjour"
-- These are all new functions, which are each waiting to receive the final parameter (`name`).
-- We can think of them behaving like this:
-- hello name = greeter "hello" name
-- or alternatively, as a lambda expression:
-- hello = (\name -> greeter "hello" name)

-- *** Function Composition using Partial Application ***

-- Let's say we have another function that formats a (last, first) name tuple into a name string:
formatName (last, first) = first ++ " " ++ last

-- We can now compose this function with any of our specialized greeting functions:
greeter' greeterFunc n = greeterFunc (formatName n)
                                      -- ^ we first apply `formatName` to our input tuple
                      -- ^ then apply any greeter function we like to the resulting output string

-- Now let's partially apply `greeter'` to the specific greetings we want:
greeterEnglish = greeter' hello
greeterSpanish = greeter' hola
greeterFrench  = greeter' bonjour
-- Note how partial application allowed us to easily compose `greeter'` with `formatName`
-- We've now performed two transformations on our original data.
-- In larger Haskell programs we'll follow this same procedure to assemble much larger compositions.
-- Try calling one of the language-specific greeters in GHCi with your name in a (last, first) tuple
-- e.x. greeterEnglish ("Curry", "Haskell"), to see how it works.