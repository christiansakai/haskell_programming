module Main where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

type TI = []

main :: IO ()
main = do
  let trigger :: TI (Int, Int, [Int])
      trigger = undefined
  quickBatch (traversable trigger)

-- 1
newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  -- fmap :: (a -> b) -> Identity a -> Identity b
  fmap f (Identity a) = Identity (f a)

instance Foldable Identity where
  -- foldr :: (a -> b -> b) -> b -> Identity a -> b
  foldr f acc (Identity a) = f a acc

instance Traversable Identity where
  -- traverse :: Applicative f => (a -> f b) -> Identity a -> f (Identity b)
  traverse f (Identity a) = pure Identity <*> f a

-- 2
newtype Constant e a =
  Constant { getConstant :: e }

instance Functor (Constant e) where
  -- fmap :: (a -> b) -> Constant e a -> Constant e b
  fmap _ (Constant e) = Constant e

instance Foldable (Constant e) where
  -- foldr :: (a -> b -> b) -> b -> Constant e a -> b
  foldr f acc (Constant e) = acc

instance Traversable (Constant e) where
  -- traverse :: Applicative f => (a -> f b) -> Constant e a -> f (Constant e b)
  traverse f (Constant e) = pure (Constant e)

-- 3
data Optional a = 
    Nada
  | Yep a

instance Functor Optional where
  -- fmap :: (a -> b) -> Optional a -> Optional b
  fmap f Nada = Nada
  fmap f (Yep a) = Yep (f a)

instance Foldable Optional where
  -- foldr :: (a -> b -> b) -> b -> Optional a -> b
  foldr f acc Nada = acc
  foldr f acc (Yep a) = f a acc

instance Traversable Optional where
  -- traverse :: Applicative f => (a -> f b) -> Optional a -> f (Optional b)
  traverse f Nada = pure Nada
  traverse f (Yep a) = pure Yep <*> f a

-- 4
data List a =
    Nil
  | Cons a (List a)

instance Monoid (List a) where
  -- mempty :: List a
  mempty = Nil

  -- mappend :: List a -> List a -> List a
  Nil `mappend` la = la
  (Cons a la) `mappend` la' = 
    Cons a $ la `mappend` la'

instance Functor List where
  -- fmap :: (a -> b) -> List a -> List b
  fmap f Nil = Nil
  fmap f (Cons a la) = Cons (f a) (fmap f la)

instance Applicative List where
  -- pure :: a -> List a
  pure a = Cons a Nil

  -- (<*>) :: List (a -> b) -> List a -> List b
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  (Cons f lf) <*> la = 
    fmap f la `mappend` (lf <*> la)

instance Foldable List where
  -- foldr :: (a -> b -> b) -> b -> List a -> b
  foldr f acc Nil = acc
  foldr f acc (Cons a la) = f a (foldr f acc la)

-- instance Traversable List where
  -- traverse :: Applicative f => (a -> f b) -> List a -> f (List b)
  -- traverse f Nil = pure Nil
  -- traverse f (Cons a la) = f a



-- 5
data Three a b c = Three a b c

instance Functor (Three e e') where
  -- fmap :: (a -> b) -> Three e e' a -> Three e e' b
  fmap f (Three e e' a) = Three e e' (f a)

instance Foldable (Three e e') where
  -- foldr :: (a -> b -> b) -> b -> Three e e' a -> b
  foldr f acc (Three _ _ a) = f a acc

instance Traversable (Three e e') where
  -- traverse :: Applicative f => (a -> f b) -> Three e e' a -> f (Three e e' b)
  traverse f (Three e e' a) = pure (Three e e') <*> f a

-- 6
data Pair a b = Pair a b

instance Functor (Pair e) where
  -- fmap :: (a -> b) -> Pair e a -> Pair e b
  fmap f (Pair e a) = Pair e (f a)

instance Foldable (Pair e) where
  -- foldr :: (a -> b -> b) -> b -> Pair e a -> b
  foldr f acc (Pair _ a) = f a acc

instance Traversable (Pair e) where
  -- traverse :: Applicative f => (a -> f b) -> Pair e a -> f (Pair e b)
  traverse f (Pair e a) = pure (Pair e) <*> f a

