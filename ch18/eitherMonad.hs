data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  -- fmap :: (a -> b) -> Sum e a -> Sum e b
  fmap f (First a)  = First a
  fmap f (Second b) = Second (f b)

instance Applicative (Sum a) where
  -- pure :: a -> Sum e a
  pure = Second

  -- (<*>) :: Sum e (a -> b) -> Sum e a -> Sum e b
  (<*>) (First a) _           = First a
  (<*>) _ (First a)           = First a
  (<*>) (Second f) (Second a) = Second (f a)

instance Monad (Sum a) where
  -- return :: a -> Sum e a
  return = Second

  -- (>>=) :: Sum e a -> (a -> Sum e b) -> Sum e b
  (>>=) (First a) _   = First a
  (>>=) (Second a) f  = f a
