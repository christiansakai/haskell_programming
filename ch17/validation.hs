data Validation e a =
    Failure e
  | Success a
  deriving (Eq, Show)

instance Functor (Validation e) where
  -- fmap :: (a -> b) -> Validation e a -> Validation e b
  fmap f (Failure e) = Failure e
  fmap f (Success a) = Success (f a)

instance Monoid e =>
         Applicative (Validation e) where
  -- pure :: a -> Validation e a
  pure = Success

  -- (<*>) :: Validation e (a -> b) -> Validation e a -> Validation e b
  (<*>) (Failure e) _ = Failure e
  (<*>) _ (Failure e) = Failure e
  (<*>) (Success f) (Success a) = Success (f a)
