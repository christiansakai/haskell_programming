{-# LANGUAGE InstanceSigs #-}

newtype Reader r a =
  Reader { runReader :: r -> a }

-- 1
myLiftA2 :: Applicative f 
         => (a -> b -> c)
         -> f a -> f b -> f c
myLiftA2 f fa fb =
  pure f <*> fa <*> fb

myLiftA2' f fa fb =
  f <$> fa <*> fb

myLiftA2'' f fa fb =
  fmap f fa fb

-- 2
asks :: (r -> a) -> Reader r a
asks f = Reader f 

-- 3 
instance Functor (Reader r) where
  -- fmap :: (a -> b) -> Reader r a -> Reader r b
  fmap f (Reader ra) = Reader $ f . ra

instance Applicative (Reader r) where
  -- pure a -> Reader r a
  pure a = Reader $ \r -> a

  -- (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  Reader fab <*> Reader ra = Reader rb
    where rb r = let f = fab r
                     a = ra r
                  in f a
