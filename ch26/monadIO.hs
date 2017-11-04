{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad.Trans.Class
import Control.Monad.IO.Class

-- 1
newtype MaybeT m a =
  MaybeT { runMaybeT :: m (Maybe a) }

instance Functor m => Functor (MaybeT m) where
  fmap :: (a -> b) -> MaybeT m a -> MaybeT m b
  fmap f = MaybeT . fmap (fmap f) . runMaybeT

instance Applicative m => Applicative (MaybeT m) where
  pure :: a -> MaybeT m a
  pure = MaybeT . pure . Just

  (<*>) :: MaybeT m (a -> b) -> MaybeT m a -> MaybeT m b
  mf <*> ma =
    MaybeT $ (<*>) <$> (runMaybeT mf) <*> (runMaybeT ma)

instance Monad m => Monad (MaybeT m) where
  return :: a -> MaybeT m a
  return = pure

  (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
  mma >>= f = MaybeT $ runMaybeT mma >>= \ma ->
    case ma of
      Just a -> runMaybeT $ f a
      Nothing -> return Nothing

instance MonadIO m => MonadIO (MaybeT m) where
  liftIO :: forall a m 
          . MonadIO m
         => IO a -> MaybeT m a
  liftIO ioa = MaybeT $ ma
    where
      _ = ioa :: IO a

      _ = liftIO :: IO a -> m a

      _ = func (liftIO ioa) :: m (Maybe a)

      func :: m a -> m (Maybe a)
      func ma = ma >>= \a -> return (Just a)

      ma :: m (Maybe a)
      ma = func (liftIO ioa)


-- 2
newtype ReaderT r m a =
  ReaderT { runReaderT :: r -> m a }

instance Functor m => Functor (ReaderT r m) where
  fmap :: (a -> b) -> ReaderT r m a -> ReaderT r m b
  fmap f rma = ReaderT $ \r -> fmap f (runReaderT rma r)

instance Applicative m => Applicative (ReaderT r m) where
  pure :: a -> ReaderT r m a
  pure a = ReaderT $ \r -> pure a

  (<*>) :: ReaderT r m (a -> b) -> ReaderT r m a -> ReaderT r m b
  rmf <*> rma = ReaderT $ \r -> ((runReaderT rmf) r) <*> ((runReaderT rma) r)

instance Monad m => Monad (ReaderT r m) where
  return :: a -> ReaderT r m a 
  return = pure

  (>>=) :: ReaderT r m a -> (a -> ReaderT r m b) -> ReaderT r m b
  rma >>= f = ReaderT $ \r ->
    ((runReaderT rma) r) >>= \a ->
      (runReaderT $ f a) r

instance MonadIO m => MonadIO (ReaderT r m) where
  liftIO :: IO a -> ReaderT r m a
  liftIO ioa = ReaderT $ \r -> liftIO ioa


-- 3
newtype StateT s m a =
  StateT { runStateT :: s -> m (a, s) }

instance Functor m => Functor (StateT s m) where
  fmap :: (a -> b) -> StateT s m a -> StateT s m b
  fmap f sma = StateT $ \s -> 
    fmap (\(a, s) -> (f a, s)) ((runStateT sma) s)

instance Applicative m => Applicative (StateT s m) where
  pure :: a -> StateT s m a
  pure a = StateT $ \s -> pure (a, s)

  (<*>) :: forall s m a b. Applicative m => StateT s m (a -> b) -> StateT s m a -> StateT s m b
  smf <*> sma = StateT $ smb'
    where
      smf' :: s -> m ((a -> b), s)
      smf' = runStateT smf

      sma' :: s -> m (a, s)
      sma' = runStateT sma

      smb' :: s -> m (b, s)
      smb' s = 
        let mf :: m ((a -> b), s)
            mf = smf' s

            ma :: m (a, s)
            ma = sma' s

            mb :: m (b, s)
            mb = 
              (fmap func mf) <*> ma


            func :: (a -> b, s) -> (a, s) -> (b, s)
            func (f, s) (a, s') = (f a, s) 

            _ = fmap :: (a -> b) 
                     -> m a 
                     -> m b
            _ = fmap :: ((a -> b, s) -> ((a, s) -> (b, s))) 
                     -> m (a -> b, s) 
                     -> m ((a, s) -> (b, s))
            _ = fmap func :: m (a -> b, s) -> m ((a, s) -> (b, s))
            _ = fmap func mf :: m ((a, s) -> (b, s))

            _ = (<*>) :: m ((a, s) -> (b, s)) -> m (a, s) -> m (b, s)


        in 
          mb

instance Monad m => Monad (StateT s m) where
  return :: a -> StateT s m a
  return = pure

  (>>=) :: StateT s m a -> (a -> StateT s m b) -> StateT s m b
  sma >>= f = StateT $ \s ->
    ((runStateT sma) s) >>= \(a, s) ->
      ((runStateT (f a)) s) >>= return
        

instance MonadIO m => MonadIO (StateT s m) where
  liftIO :: forall a m. MonadIO m => IO a -> StateT s m a
  liftIO ioa = StateT $ \s -> 
    liftIO ioa >>= \a -> return (a, s)
