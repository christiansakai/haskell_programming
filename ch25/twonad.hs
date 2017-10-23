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

instance ( Monad f
         , Monad g
         ) => Monad (Compose f g) where
  return :: a -> Compose f g a
  return = pure

  (>>=) :: Compose f g a
        -> (a -> Compose f g b)
        -> Compose f g b
  Compose fga >>= fun =
    -- impossible to write
    undefined
