import Data.Monoid

-- 1
data Constant a b = Constant a b

instance Foldable (Constant e) where
  -- foldMap :: Monoid m => (a -> m) -> Constant e a -> m
  foldMap aTom (Constant _ a) = aTom a
  
  -- foldr :: (a -> b -> b) -> b -> Constant e a -> b
  foldr f acc (Constant _ a) = f a acc

  -- foldl :: (b -> a -> b) -> b -> Constant e a -> b
  foldl f acc (Constant _ a) = f acc a

-- 2
data Two a b = Two a b

instance Foldable (Two e) where
  -- foldMap :: Monoid m => (a -> m) -> Two e a -> m
  foldMap aTom (Two _ a) = aTom a

  -- foldr :: (a -> b -> b) -> b -> Two e a -> b
  foldr f acc (Two e a) = f a acc

  -- foldl :: (b -> a -> b) -> b -> t a -> b
  foldl f acc (Two e a) = f acc a

-- 3
data Three a b c = Three a b c

instance Foldable (Three a b) where
  -- foldMap :: Monoid m => (a -> m) -> Three e e' a -> m
  foldMap aTom (Three _ _ a) = aTom a

  -- foldr :: (a -> b -> b) -> b -> Three e e' a -> b
  foldr f acc (Three _ _ a) = f a acc

  -- foldl :: (b -> a -> b) -> b -> Three e e' a -> b
  foldl f acc (Three _ _ a) = f acc a

-- 4
data Three' a b = Three' a b b

instance Foldable (Three' a) where
  -- foldMap :: Monoid m => (a -> m) -> Three' e a -> m
  foldMap aTom (Three' _ a a') = aTom a <> aTom a'

  -- foldr :: (a -> b -> b) -> b -> Three' e a -> b
  foldr f acc (Three' _ a a') = f a (f a' acc)

  -- foldl :: (b -> a -> b) -> b -> Three' e a -> b
  foldl f acc (Three' _ a a') = f (f acc a) a'

-- 5
data Four' a b = Four' a b b b

instance Foldable (Four' a) where
  -- foldMap :: Monoid m => (a -> m) -> Four' e a -> m
  foldMap aTom (Four' _ a a' a'') = aTom a <> aTom a' <> aTom a''

  -- foldr :: (a -> b -> b) -> b -> Four' e a -> b
  foldr f acc (Four' _ a a' a'') = f a (f a' (f a'' acc))

  -- foldl :: (b -> a -> b) -> b -> Four' e a -> b
  foldl f acc (Four' _ a a' a'') = f (f (f acc a'') a') a
