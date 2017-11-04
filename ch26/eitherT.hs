{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

-- 2
instance Applicative m => Applicative (EitherT e m) where
  pure :: a -> EitherT e m a
  pure a = EitherT $ pure (Right a)

  (<*>) :: forall e m a b . 
           Applicative m
        => EitherT e m (a -> b) -> EitherT e m a -> EitherT e m b
  (EitherT emf) <*> (EitherT ema) =
    EitherT $ fmap (<*>) emf <*> ema

-- 3
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

-- 4
swapEither :: Either e a -> Either a e
swapEither (Left e) = Right e
swapEither (Right a) = Left a

swapEitherT :: forall e a m
            . (Functor m)
            => EitherT e m a
            -> EitherT a m e
swapEitherT (EitherT mea) = 
  EitherT $ fmap swapEither mea

-- 5
eitherT :: forall a b c m
         . Monad m 
        => (a -> m c)
        -> (b -> m c)
        -> EitherT a m b
        -> m c
eitherT aTomc bTomc (EitherT mab) = 
  mab >>= \eitherAb ->
    case eitherAb of
      Left a -> aTomc a
      Right b -> bTomc b
