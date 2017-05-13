-- 1

-- 2
data Two a b =
  Two a b

instance Foldable (Two a) where
  -- foldr :: (b -> c -> c) -> c -> Two a b -> c
  foldr f acc (Two a b) = f b acc

  -- foldl :: (c -> b -> c) -> c -> Two a b -> c
  foldl f acc (Two a b) = f acc b
  
  -- foldMap :: Monoid m => (b -> m) -> Two a b -> m
  foldMap f (Two a b) = f b


-- 3
data Three a b c =
  Three a b c

instance Foldable (Three a b) where
  -- foldr :: (c -> d -> d) -> d -> Three a b c -> d
  foldr f acc (Three a b c) = f c acc
  
  -- foldl :: (d -> c -> d) -> d -> Three a b c -> d
  foldl f acc (Three a b c) = f acc c

  -- foldMap :: Monoid m => (c -> m) -> Three a b c -> m
  foldMap f (Three a b c) = f c


-- 4
