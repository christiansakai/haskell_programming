newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  -- fmap :: (a -> b) -> Identity a -> Identity b
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  -- pure :: a -> Identity a
  pure = Identity
  
  -- (<*>) :: Identity (a -> b) -> Identity a -> Identity b
  (Identity f) <*> (Identity a) = Identity (f a)
