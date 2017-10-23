{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

newtype MaybeT m a =
  MaybeT { runMaybeT :: m (Maybe a) }

instance Functor m => Functor (MaybeT m) where
  fmap :: Functor m => (a -> b) -> MaybeT m a -> MaybeT m b
  fmap f (MaybeT ma) = 
    MaybeT $ (fmap . fmap) f ma

instance Applicative m => Applicative (MaybeT m) where
  pure :: a -> MaybeT m a
  pure x = MaybeT (pure (pure x))

  (<*>) :: MaybeT m (a -> b) -> MaybeT m a -> MaybeT m b
  (MaybeT fab) <*> (MaybeT mma) = 
    MaybeT $ (<*>) <$> fab <*> mma

instance (Monad m) => Monad (MaybeT m) where
  return :: a -> MaybeT m a
  return = pure

  (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
  (MaybeT ma) >>= f = 
    MaybeT $ do
      v <- ma
      case v of
        Nothing -> return Nothing
        Just y -> runMaybeT (f y)

