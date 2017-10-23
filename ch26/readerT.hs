{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

newtype ReaderT r m a =
  ReaderT { runReaderT :: r -> m a }

instance Functor m => Functor (ReaderT r m) where
  fmap :: forall r a b m
        . Functor m
       => (a -> b) -> ReaderT r m a -> ReaderT r m b
  fmap f (ReaderT rma) = 
    ReaderT rmb
      where 
        _ = f :: a -> b
        _ = rma :: r -> m a
        rmb :: r -> m b
        rmb r = 
          fmap f (rma r)
         
instance Applicative m => Applicative (ReaderT r m) where
  pure :: a -> ReaderT r m a
  pure a = ReaderT rma
    where
      rma r = pure a

  (<*>) :: forall r m a b 
         . Applicative m
        => ReaderT r m (a -> b) -> ReaderT r m a -> ReaderT r m b
  ReaderT rmf <*> ReaderT rma = 
    ReaderT rmb
      where
        _ = rmf :: r -> m (a -> b)
        _ = rma :: r -> m a
        rmb :: r -> m b
        rmb r = 
          let mab = rmf r
              ma = rma r
              mb :: m b
              mb = mab <*> ma
          in mb

instance Monad m => Monad (ReaderT r m) where
  return :: Monad m => a -> ReaderT r m a
  return a = ReaderT rma
    where
      rma r = return a

  (>>=) :: forall r m a b
         . Monad m 
        => ReaderT r m a 
        -> (a -> ReaderT r m b) 
        -> ReaderT r m b
  ReaderT rma >>= f = 
    ReaderT $ \r -> do
      a <- rma r
      runReaderT (f a) r
