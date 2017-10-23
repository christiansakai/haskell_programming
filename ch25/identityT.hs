newtype Identity a = 
  Identity { runIdentity :: a }
  deriving (Eq, Show)

newtype IdentityT f a = 
  IdentityT { runIdentityT :: f a }

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Functor f => Functor (IdentityT f) where
  fmap f (IdentityT fa) = IdentityT (fmap f fa)

instance Applicative Identity where
  pure = Identity

  (Identity f) <*> (Identity a) = Identity (f a)

instance Applicative f => Applicative (IdentityT f) where
  -- pure :: a -> IdentityT f a
  pure a = IdentityT (pure a)

  -- (<*>) :: IdentityT f (a -> b) -> IdentityT f a
  (IdentityT ffab) <*> (IdentityT fa) =
    IdentityT (ffab <*> fa)

instance Monad Identity where
  -- return :: a -> Identity a
  return = pure

  -- (>>=) ::Identity a -> (a -> Identity b) -> Identity b
  Identity a >>= f = f a

instance Monad m => Monad (IdentityT m) where
  -- return :: a -> IdentityT m a
  return = pure

  -- (>>=) :: IdentityT m a -> (a -> IdentityT m b) -> IdentityT m b
  (IdentityT ma) >>= f = 
    IdentityT $ ma >>= runIdentityT . f





