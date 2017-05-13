-- 1
j :: Monad m => m (m a) -> m a
j mma = mma >>= \ma -> ma

-- 2
l1 :: Monad m => (a -> b) -> m a -> m b
l1 f ma = ma >>= (\a -> return (f a))

-- 3
l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f ma mb =
  ma >>= 
    \a ->
      mb >>= 
        \b ->
          return (f a b)

-- 4
a :: Monad m => m a -> m (a -> b) -> m b
a ma mf =
  ma >>= 
    \a ->
      mf >>= 
        \f ->
          return (f a)

-- 5
