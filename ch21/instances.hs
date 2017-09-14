import Prelude hiding 
  ( Either(..))

data Either a b = 
    Left a
  | Right b
  deriving (Eq, Ord, Show)

instance Functor (Either e) where
  -- fmap :: (a -> b) -> Either e a -> Either e b
  fmap f (Left e) = Left e
  fmap f (Right a) = Right $ f a

instance Foldable (Either e) where
  -- foldMap :: Monoid m => (a -> m) -> Either e a -> m
  foldMap f (Left e) = mempty
  foldMap f (Right a) = f a

  -- foldr :: (a -> b -> b) -> b -> Either e a -> b
  foldr f b (Left e) = b
  foldr f b (Right a) = f a b

instance Traversable (Either e) where
  -- traverse :: Applicative f => (a -> f b) -> Either e a -> f (Either e b)
  traverse f (Left e) = pure $ Left e
  traverse f (Right a) = fmap Right (f a)


data Tuple a b = Tuple a b
  deriving (Eq, Ord, Show)

instance Functor (Tuple e) where
  -- fmap :: (a -> b) -> Tuple e a -> Tuple e b
  fmap f (Tuple e a) = Tuple e (f a)

instance Foldable (Tuple e) where
  -- foldMap :: Monoid m => (a -> m) -> Tuple e a -> m
  foldMap f (Tuple e a) = f a

  -- foldr :: (a -> b -> b) -> b -> Tuple e a -> b
  foldr f b (Tuple e a) = f a b 

instance Traversable (Tuple e) where
  -- traverse :: Applicative f => (a -> f b) -> Tuple e a -> f (Tuple e b)
  traverse f (Tuple e a) = fmap (Tuple e) (f a)
  


