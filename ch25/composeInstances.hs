{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

newtype Compose f g a =
  Compose { getCompose :: f (g a) }

instance ( Functor f
         , Functor g
         ) => Functor (Compose f g) where
  fmap :: (a -> b) -> Compose f g a -> Compose f g b
  fmap fun (Compose fga) = Compose $ (fmap . fmap) fun fga

instance ( Applicative f
         , Applicative g
         ) => Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure a = Compose (pure (pure a))

  (<*>) :: forall a b f g . 
        (Applicative f, Applicative g) 
        => Compose f g (a -> b)
        -> Compose f g a
        -> Compose f g b
  Compose fun <*> Compose fga = 
    Compose $ fmap (<*>) fun <*> fga

-- 1
instance ( Foldable f
         , Foldable g
         ) => Foldable (Compose f g) where
  foldMap :: Monoid m
          => (a -> m) -> Compose f g a -> m
  foldMap aTom (Compose fga) = 
    foldMap gaTom fga
      where
        gaTom ga = foldMap aTom ga

-- 2
instance ( Traversable f
         , Traversable g
         ) => Traversable (Compose f g) where
  traverse :: Applicative z 
           => (a -> z b) -> Compose f g a -> z (Compose f g b)
  traverse aTozb (Compose fga) = 
    undefined


