{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad.Trans.Class

-- 1
newtype EitherT e m a =
  EitherT { runEitherT :: m (Either e a) }

instance MonadTrans (EitherT e) where
  lift :: forall e m a
        . Monad m 
       => m a -> EitherT e m a
  lift ma = EitherT mea
    where
      mea :: m (Either e a)
      mea = ma >>= \a ->
        return $ Right a
        
-- 2
newtype StateT s m a =
  StateT { runStateT :: s -> m (a, s) }

instance MonadTrans (StateT s) where
  lift :: forall e m a s
        . Monad m
       => m a -> StateT s m a
  lift ma = StateT smas
    where
      smas :: s -> m (a, s)
      smas s = ma >>= \a -> return (a, s)

