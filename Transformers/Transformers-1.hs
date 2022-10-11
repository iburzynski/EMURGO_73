{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
import Control.Monad (join)
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use <&>" #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# HLINT ignore "Redundant bracket" #-}

-- *** Composing Contexts: Functors, Applicatives & Monads ***

-- We've learned how to easily work with data inside a variety of contexts using the four "adapter"
--   methods that define the Functor, Applicative, and Monad typeclasses:
--   * fmap
--   * pure
--   * <*> ("app")
--   * >>= ("bind")
-- These methods helped us resolve all of the possible type mismatches we encounter when performing
--   computation inside a single context, allowing us to write concise and composable code.

-- However, most non-trivial applications will require the use of multiple contexts, which are often
--   nested together in complex "stacks" of overlapping functionality.
-- Ex. Functions that have the possibility of failure or returning null values, which are also
--   interfacing with the outside world require use of the IO context plus Either or Maybe.
-- These stacks can become quite large and unwieldy to work with, so it would be nice if we can use
--   the same helpful adapters to work with them as conveniently as we can with a single context.

-- As an example, we'll attempt to create a new data type `Compose`, which wraps a value of any type
--   inside any two nested layers of context `g` and `f`:
newtype Compose g f a = Compose { getCompose :: g (f a) }
  deriving (Eq, Show)

-- We'll then attempt to define typeclass instances for `Compose` to gain use of their methods.

-- The Functor instance turns out to be trivial, because composing two datatypes with their own
--   Functor instances always gives rise to a new Functor instance (in math terms, we say that
--   Functors are "closed under composition").

-- We can produce versions of functions capable of transforming data inside arbitrarily nested
--   contexts simply by composing `fmap` with itself multiple times:
-- fmap . fmap        :: (a -> b) ->    g (f a)  ->    g (f b)
-- fmap . fmap . fmap :: (a -> b) -> h (g (f a)) -> h (g (f b))
-- As long as every context in the stack is a Functor, we can map over as many layers as necessary.

instance (Functor g, Functor f) => Functor (Compose g f) where
  fmap :: (a -> b) -> Compose g f a -> Compose g f b
  fmap ab (Compose gfa) = Compose $ (fmap . fmap) ab gfa

-- Similarly, we can compose two datatypes with Applicative instances to get a new Applicative,
--   because Applicative Functors are also "closed under composition".

instance (Applicative g, Applicative f) => Applicative (Compose g f) where
-- We can promote a value into nested contexts by composing `pure` with itself:
-- pure . pure        :: a ->    g (f a)
-- pure . pure . pure :: a -> h (g (f a))
  pure :: a -> Compose g f a
  pure a = Compose $ (pure . pure) a
-- Point-free version:
-- pure = Compose . pure . pure

-- Defining "app" is more complicated, but still achievable.
-- It requires two steps:
--   1. Lift "app" over the outer context `g` via `fmap`, converting `f (a -> b)` to `f a -> f b`
--           (<*>) ::    f (a -> b)  ->   (f a -> f b)
--      fmap (<*>) :: g (f (a -> b)) -> g (f a -> f b)
--                                         ^ (<*>) is lifted over `g` and applied to `f (a -> b)`

--   2. Now the Applicative instance for `g` can be used to apply the resulting `f a -> f b` to an
--      `f a` argument inside the context of `g`.
--           (<*>) :: g (f a -> f b) -> g (f a) -> g (f b)
  (<*>) :: Compose g f (a -> b) -> Compose g f a -> Compose g f b
  Compose gfab <*> Compose gfa = Compose $ fmap (<*>) gfab <*> gfa
--                                         ^ Step 1: map "app" onto `f (a -> b)` (result :: g (f a -> f b))
--                                                         ^ Step 2: apply `g (f a -> f b)` to
--                                                           `g (f a)` with "app"

-- Because Applicatives are easily composable and provide a lot of power on their own,
-- sometimes using applicative operations is preferable if we don't need the full power of a Monad.

-- As we're about to discover, Monads aren't closed under composition like Functors/Applicatives:

instance (Monad g, Monad f) => Monad (Compose g f) where
  (>>=) :: Compose g f a -> (a -> Compose g f b) -> Compose g f b
  Compose gfa >>= aCgfb = Compose $ do
    fa <- gfa -- get `fa` out of its `g` context...
    -- we need a value `a` to pass to `aCgfb`, but binding `fa` doesn't work:
    -- (>>=) :: f a -> (a -> f b) -> f b
    let fCgfb = fa >>= pure . aCgfb -- :: f (Compose (g (f b)))
    --  We now have our desired return type enclosed in an extra layer of `f` context...
    --  We can remove the misplaced `Compose` constructor layer by fmapping `getCompose`:
        fgfb  = getCompose <$> fCgfb  -- :: f (g (f b))
    --  But we have no way of removing that outer `f` layer.
    --  If the two `f` layers were side by side (g (f (f b))), we could use the monadic `join` method to collapse
    --   them together: join :: Monad m => m (m a) -> m a
    --  But we'd need to swap the order of the `f` and `g` layers, and we have no way to do so.
    --  We also can't use pattern-matching/destructuring to escape the offending context, because we
    --    don't know anything about `f` or `g`: we've designed `Compose` to accept any two arbitrary
    --    contexts, so we don't have any information about their constructors (and remember, we
    --    couldn't pattern match on IO in any case since its constructor isn't exposed).
    return undefined

-- How do we deal with this obstacle? Do we need to produce custom one-off Monads for every possible
--  combination of contexts?

-- The final point in our attempt to implement the Monad instance above gives us a clue about how we
--   might proceed if we wish to compose two monads: maybe if we had more information about one of
--   the contexts (`f`), we could use pattern-matching to liberate ourselves from the redundant `f`.
-- This would allow us to produce a generic Monad generator for each specific context (except IO).
-- These generators, called "Monad Transformers", can be used to easily compose monads on the fly.

-- *** Monad Transformers ***
--   * We can get a Monad for two types as long as we know what one of them is.
--   * Transformers let us make a monad out of multiple types that each have a Monad instance,
--     by wrapping around existing monads that provide each bit of desired functionality.

-- A Monad Transformer is a type constructor that:
--   * takes a (unspecified) Monad as an argument (`m`)
--   * returns a new Monad as a result (i.e. `MaybeT m`)

-- Note: the name of the transformer (`MaybeT`) comes from the *inner* monad, whose type is known
newtype MaybeT n a = MaybeT { runMaybeT :: n (Maybe a) }
-- `MaybeT` is a Maybe value, in some other (unknown) context that has a Monad instance
-- The other Monad, called the "base monad", remains polymorphic as an argument to the constructor.
-- Note that, somewhat counterintuitively, the base monad is structurally the outermost context.

instance Functor n => Functor (MaybeT n) where
  fmap :: (a -> b) -> MaybeT n a -> MaybeT n b -- MaybeT (n (Maybe b))
  fmap ab (MaybeT nma) = MaybeT $ (fmap . fmap) ab nma

instance Applicative n => Applicative (MaybeT n) where
  pure :: a -> MaybeT n a
  pure = MaybeT . pure . pure

  (<*>) :: MaybeT n (a -> b) -> MaybeT n a -> MaybeT n b
  MaybeT nmab <*> MaybeT nma = MaybeT $ fmap (<*>) nmab <*> nma

instance Monad n => Monad (MaybeT n) where
  (>>=) :: MaybeT n a -> (a -> MaybeT n b) -> MaybeT n b -- MaybeT (n (Maybe b))
  MaybeT nma >>= aMnmb = MaybeT $ do
    ma <- nma
    case ma of
      Nothing -> pure Nothing
      Just a  -> runMaybeT $ aMnmb a -- MaybeT (MaybeT (n (Maybe b))) <- remove extra `MaybeT`
      --                                                                 constructor via `runMaybeT`

-- *** The MonadTrans typeclass ***
--   * Has one core method, `lift`: lifts a computation from the argument Monad to a transformer
class MonadTrans t where
  lift :: (Monad n) => n a -> t n a

instance MonadTrans MaybeT where
  lift :: (Monad n) => n a -> MaybeT n a
  lift na   = MaybeT $ fmap pure na
  -- same as: MaybeT $ fmap Just na

-- Further Reading:
-- Chris Allen & Julie Moronuki: Haskell Programming from First Principles
-- Chapter 25: Composing Types & 26: Monad Transformers