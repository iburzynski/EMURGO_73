{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use =<<" #-}
import           Control.Monad (join)

newtype Compose n m a = Compose { getCompose :: n (m a) } deriving Show

instance (Functor m, Functor n) => Functor (Compose n m) where
  fmap :: (a -> b) -> Compose n m a -> Compose n m b
  -- f :: (a -> b)
  -- x :: n (m a)
  fmap f (Compose x) = Compose $ (fmap . fmap) f x

instance (Applicative m, Applicative n) => Applicative (Compose n m) where
  pure :: a -> Compose n m a
  pure x = Compose $ (pure . pure) x

  (<*>) :: Compose n m (a -> b) -> Compose n m a -> Compose n m b
  -- f :: n (m (a -> b)) => n (m a -> m b)
  -- x :: n (m a)

  -- (<*>) :: f (a -> b) -> (f a -> f b)
  Compose f <*> Compose x = Compose $ fmap (<*>) f <*> x
--                                    ^ n (m a -> m b)
--                                                     ^ n (m a)

instance (Monad m, Monad n, Traversable m, Traversable n) => Monad (Compose n m) where
  (>>=) :: Compose n m a -> (a -> Compose n m b) -> Compose n m b
  -- x :: n (m a)
  -- k :: a -> Compose n m b
  -- x' :: m a
  -- y :: m (n m b)
  -- z :: m (m n b)
  Compose x >>= k = Compose $ do
    x' <- x
    let y = getCompose . k <$> x'
        z = join (sequenceA <$> y)
    sequenceA z

newtype MaybeT n a = MaybeT { runMaybeT :: n (Maybe a) }

instance (Functor n) => Functor (MaybeT n) where
  fmap f (MaybeT x) = MaybeT $ (fmap . fmap) f x

instance (Applicative n) => Applicative (MaybeT n) where
  pure :: a -> MaybeT n a
  pure x = MaybeT $ (pure . pure) x

  (<*>) :: MaybeT n (a -> b) -> MaybeT n a -> MaybeT n b
  -- f :: n (m (a -> b)) => n (m a -> m b)
  -- x :: n (m a)
  -- (<*>) :: f (a -> b) -> (f a -> f b)
  MaybeT f <*> MaybeT x = MaybeT $ fmap (<*>) f <*> x

instance (Monad n) => Monad (MaybeT n) where
  (>>=) :: MaybeT n a -> (a -> MaybeT n b) -> MaybeT n b
  -- x :: n (Maybe a)
  -- k :: a -> MaybeT n b
  MaybeT x >>= k = MaybeT $ do
    my <- x
    case my of
      Nothing -> pure Nothing
      Just y  -> runMaybeT $ k y

class MonadTrans n where
  lift :: Monad m => m a -> n m a

instance MonadTrans MaybeT where
