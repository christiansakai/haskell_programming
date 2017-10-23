{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad.Trans.Class

-- 1
newtype EitherT e m a =
  EitherT { runEitherT :: m (Either e a) }

instance Functor m => Functor (EitherT e m) where
  fmap :: forall a b e m . 
          Functor m 
       => (a -> b) -> EitherT e m a -> EitherT e m b
  fmap f (EitherT ema) =
    EitherT $ fmap doFunc ema
      where
        doFunc either =
          case either of
            Left e -> Left e
            Right a -> Right (f a)

instance Applicative m => Applicative (EitherT e m) where
  pure :: a -> EitherT e m a
  pure a = EitherT $ pure (Right a)

  (<*>) :: forall e m a b . 
           Applicative m
        => EitherT e m (a -> b) -> EitherT e m a -> EitherT e m b
  (EitherT emf) <*> (EitherT ema) =
    EitherT $ fmap (<*>) emf <*> ema

instance Monad m => Monad (EitherT e m) where
  return :: a -> EitherT e m a
  return a = EitherT $ return (Right a)

  (>>=) :: forall e m a b .
           Monad m => 
           EitherT e m a -> (a -> EitherT e m b) -> EitherT e m b
  EitherT ema >>= f =
    EitherT $ ema >>= fun
      where
        fun either =
          case either of
            Left e -> 
              return $ Left e
            Right a -> 
              runEitherT (f a)

instance MonadTrans (EitherT e) where
  lift :: forall m e a 
        . Monad m 
       => m a -> EitherT e m a
  lift ma = EitherT mEitherea
    where 
      _ = return :: a -> m a
      _ = (>>=) :: m a -> (a -> m (Either e a)) -> m (Either e a)

      mEitherea :: m (Either e a)
      mEitherea = 
        ma >>= \a ->
          return $ Right a

-- 2
newtype StateT s m a =
  StateT { runStateT :: s -> m (a, s) }

instance Functor m => Functor (StateT s m) where
  fmap :: forall a b s m 
        . Functor m
       => (a -> b) -> StateT s m a -> StateT s m b
  fmap f (StateT sma) =
    StateT smb
      where
        _ = fmap :: (a -> b) -> m a -> m b
        _ = f :: (a -> b)
        _ = sma :: s -> m (a, s)
        _ = (f . fst) :: (a, s) -> b
        _ = fmap (f . fst) :: m (a, s) -> m b

        smb :: s -> m (b, s)
        smb s =
          let mas :: m (a, s)
              mas = sma s

              fun :: m (a, s) -> m (b, s)
              fun = fmap (\as ->
                let a = fst as
                    s = snd as
                 in (f a, s))

           in fun mas

instance Monad m => Applicative (StateT s m) where
  pure :: forall s m a 
        . Monad m
       => a -> StateT s m a
  pure a = StateT sma
    where sma :: s -> m (a, s)
          sma s = return (a, s)

  (<*>) :: forall s m a b
         . Monad m
        => StateT s m (a -> b) -> StateT s m a -> StateT s m b
  StateT smab <*> StateT sma = 
    StateT smb
      where
        _ = smab :: s -> m ((a -> b), s)
        _ = sma :: s -> m (a, s)
        _ = return :: a -> m a
        _ = (>>=) :: m a -> (a -> m b) -> m b

        smb :: s -> m (b, s)
        smb s =
          let mas :: m (a, s)
              mas = sma s

              mabs :: m ((a -> b), s)
              mabs = smab s

          in 
            mas >>= \(a, s) ->
              mabs >>= \(aTob, s) ->
                return (aTob a, s)

instance Monad m => Monad (StateT s m) where
  return :: a -> StateT s m a
  return = pure

  (>>=) :: forall s m a b
         . Monad m
        => StateT s m a -> (a -> StateT s m b) -> StateT s m b
  StateT sma >>= f =
    StateT smb
      where
        _ = sma :: s -> m (a, s)
        _ = f :: a -> StateT s m b
        _ = return :: a -> m a
        _ = (>>=) :: m a -> (a -> m b) -> m b

        smb :: s -> m (b, s)
        smb s = 
          let mas :: m (a, s)
              mas = sma s

          in 
            mas >>= \(a, s) ->
              let stateTsmb :: StateT s m b
                  stateTsmb = f a

                  smb :: s -> m (b, s)
                  smb = runStateT stateTsmb

              in smb s

instance MonadTrans (StateT s) where
  lift :: forall s m a 
        . Monad m 
       => m a -> StateT s m a
  lift ma = StateT sma
    where
      _ = return :: a -> m a
      _ = (>>=) :: m a -> (a -> m (a, s)) -> m (a, s)

      sma :: s -> m (a, s)
      sma s = 
        ma >>= \a ->
          return (a, s)

