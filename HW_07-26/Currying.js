// Let's define two versions of a simple function with arity 3.
// It just takes three values and sums them.

// `uncurried` is a version that takes 3 arguments (arity 3) and returns their sum
function uncurried(a, b, c) {
  return a + b + c;
}

// `curried` is a version that takes 1 argument `a` and returns a new function `curried2`.
// `curried2` takes 1 new argument `b` and returns a new function `curried3`.
// `curried3` takes 1 final argument `c` and adds it to the two previous values `a` and `b`.
// The final function `curried3` is able to access the values `a` and `b` via closure.
// Think of it like this:
// `curried` puts value `a` into its backpack, which gets passed on to `curried2`
// `curried2` puts value `b` into its backpack, which gets passed on to `curried3`
// `curried3` unpacks its backpack and performs the final computation.
// Each new function "closes over" the argument it just received, storing it inside its backpack.
function curried(a) {
  return function curried2(b) {
    return function curried3(c) {
      return a + b + c;
    };
  };
}

// The two versions produce the same result, but must be called differently in JavaScript:
// The uncurried version must receive all of its arguments at once, or it will return `NaN`:
uncurried(1, 2, 3);
// The curried version can be given less arguments than the total.
// In such cases, it will return a new function that waits to receive additional arguments.
curried(1)(2)(3);
const curried2 = curried(1);
const curried3 = curried2(2); // or: curried(1)(2)
const result = curried3(3); // or: curried(1)(2)(3), curried2(2)(3) -- all produce the same result

// Another key difference is the functions' arities:
// `uncurried` has arity 3, while `curried` has arity 1.
// The intermediate functions `curried2` and `curried3` also have arity 1.

// Ignoring the clumsiness of currying a function in JavaScript, why would this be useful?

// 1. It unlocks *partial application* of arguments to functions.
//    This allows us to dynamically create multiple specific versions of a more general function.
//    Say we have one value `a`, and a list of 1000 pairs of `b` and `c` values.
//    We can apply `curried` to `a` a single time to create a more specialized function,
//    then use that function with our list of pairs, to avoid repeating ourselves.

// 2. It makes multi-argument functions *composable*.
//    Functions can have many input values, but only one output value.
//    We've learned that function composition is the essence of functional programming.
//    We build many tiny, reusable functions and chain them together into pipelines.
//    We can only pass the output of a function to the next function if that function has arity 1.
//    Currying transforms a multi-argument function into a sequence of unary functions
//    which means we can partially apply arguments until each function in our pipeline has arity 1.
