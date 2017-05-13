data Identity a =
  Identity a

instance Foldable Identity where
  -- foldr :: (a -> b -> b) -> b -> Identity a -> b
  foldr f acc (Identity x) = f acc x

  -- foldl :: (b -> a -> b) -> b -> Identity a -> b
  foldl f acc (Identity x) = f x acc

  -- foldMap :: Monoid m => (a -> m) -> Identity a -> m
  foldMap f (Identity x) = f x

