{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

