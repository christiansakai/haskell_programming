{-# LANGUAGE InstanceSigs #-}

newtype Reader r a =
  Reader { runReader :: r -> a }

instance Functor (Reader r) where
  fmap :: (a -> b) -> Reader r a -> Reader r b
  fmap f (Reader ra) = Reader $ f . ra

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader $ \r -> a

  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  Reader fab <*> Reader ra = Reader rb
    where rb r = let f = fab r
                     a = ra r
                  in f a

instance Monad (Reader r) where
  return :: a -> Reader r a
  return a = Reader $ const a

  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  -- Reader ra >>= f = f . ra
  (Reader ra) >>= f = Reader rb
    where rb r = runReader (f $ ra r) $ r
