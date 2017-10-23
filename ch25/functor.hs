{-# LANGUAGE InstanceSigs #-}

newtype Identity a =
  Identity { runIdentity :: a }

instance Functor Identity where
  fmap :: (a -> b) -> Identity a -> Identity b
  fmap f (Identity a) = Identity (f a)

newtype Compose f g a =
  Compose { getCompose :: f (g a) }

instance ( Functor f
         , Functor g) => Functor (Compose f g) where
  fmap :: (a -> b) -> Compose f g a -> Compose f g b
  fmap func (Compose fga) = Compose $ (fmap . fmap) func fga

newtype One f a =
  One (f a)

instance Functor f => Functor (One f) where
  fmap :: (a -> b) -> One f a -> One f b
  fmap func (One fa) = One $ fmap func fa

newtype Three f g h a =
  Three (f (g (h a)))

instance ( Functor f
         , Functor g
         , Functor h
         ) => Functor (Three f g h) where
  fmap :: (a -> b) -> Three f g h a -> Three f g h b
  fmap func (Three fgha) = Three $ (fmap . fmap . fmap) func fgha

