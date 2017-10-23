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
    Compose $ (fmap (<*>) fun) <*> fga
      where
        -- Do not delete
        -- This is how I came up with the answer
        -- by using type hole debugging
        _ = fun :: f (g (a -> b))
        _ = fga :: f (g a)
        _ = (<*>) :: f (a -> b) -> f a -> f b
        _ = fmap (<*>) :: f (g (a -> b)) -> f (g a -> g b)
        _ = fmap (<*>) fun :: f (g a -> g b)
        _ = (fmap (<*>) fun) <*> fga :: f (g b)

data Hole = Hole
