#!/usr/local/bin/stack
{- stack 
   exec ghci
   --resolver lts-9.3 
   --install-ghc 
   --package QuickCheck
   --package checkers
-}

{-# LANGUAGE FlexibleContexts #-}

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

instance Functor Identity where
  -- fmap :: (a -> b) -> Identity a -> Identity b
  fmap f (Identity a) = Identity $ f a

instance Foldable Identity where
  -- foldMap :: Monoid m => (a -> m) -> Identity a -> m
  foldMap f (Identity a) = f a

  -- foldr :: (a -> b -> b) -> b -> Identity a -> b
  foldr f b (Identity a) = f a b 

instance Traversable Identity where
  -- traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
  traverse f (Identity a) = fmap Identity (f a)


-- 2
newtype Constant a b = 
  Constant { getConstant :: a }

instance Functor (Constant e) where
  -- fmap :: (a -> b) -> Constant e a -> Constant e b
  fmap f (Constant e) = Constant e

instance Foldable (Constant e) where
  -- foldMap :: Monoid m => (a -> m) -> Constant e a -> m
  foldMap f (Constant e) = mempty

  -- foldr :: (a -> b -> b) -> b -> Constant e a -> b
  foldr f b (Constant e) = b

instance Traversable (Constant a) where
  -- traverse :: Applicative f => (a -> f b) -> Constant e a -> f (Constant e b)
  traverse f (Constant e) = pure $ Constant e


-- 3
data Optional a = Nada
                | Yep a

instance Functor Optional where
  -- fmap :: (a -> b) -> Optional a -> Optional b
  fmap f Nada = Nada
  fmap f (Yep a) = Yep (f a)

instance Foldable Optional where
  -- foldMap :: Monoid m => (a -> m) -> Optional a -> m
  foldMap f Nada = mempty
  foldMap f (Yep a) = f a

  -- foldr :: (a -> b -> b) -> b -> Optional a -> b
  foldr f b Nada = b
  foldr f b (Yep a) = f a b

instance Traversable Optional where
  -- traverse :: Applicative f => (a -> f b) -> Optional a -> f (Optional b)
  traverse f Nada = pure Nada
  traverse f (Yep a) = fmap Yep (f a)


-- 4
data List a = Nil
            | Cons a (List a)

instance Monoid (List a) where
  -- mempty :: List a
  mempty = Nil

  -- mappend :: List a -> List a -> List a
  Nil `mappend` la = la
  la `mappend` Nil = la
  (Cons a la) `mappend` la' = Cons a $ la `mappend` la'

instance Functor List where
  -- fmap :: (a -> b) -> List a -> List b
  fmap f Nil = Nil
  fmap f (Cons a la) = Cons (f a) (fmap f la)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys =
  Cons x $ xs `append` ys

instance Applicative List where
  -- pure :: a -> List a
  pure a = Cons a Nil

  -- (<*>) :: List (a -> b) -> List a -> List b
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  (Cons f lf) <*> la  = 
    fmap f la `append` (lf <*> la)


instance Foldable List where
  -- foldr :: (a -> b -> b) -> b -> List a -> b
  foldr f b Nil = b
  foldr f b (Cons a la) = f a (foldr f b la) 

-- definitely wrong
instance Traversable List where
  -- traverse :: Applicative f => (a -> f b) -> List a -> f (List b)
  traverse f Nil = pure Nil
  traverse f (Cons a la) = 
    pure Cons <*> f a <*> traverse f la
  -- traverse f (Cons a la) = fmap doCons (f a)
    -- where doCons a = Cons a Nil


-- 5
data Three a b c = Three a b c

instance Functor (Three e e') where
  -- fmap :: (a -> b) -> Three e e' a -> Three e e' b
  fmap f (Three e e' a) = Three e e' (f a)

instance Foldable (Three e e') where
  -- foldMap :: Monoid m => (a -> m) -> Three e e' a -> m
  foldMap f (Three e e' a) = f a

  -- foldr :: (a -> b -> b) -> b -> Three e e' a -> b
  foldr f b (Three e e' a) = f a b

instance Traversable (Three e e') where
  -- traverse :: Applicative f => (a -> f b) -> Three e e' a -> f (Three e e' b)
  traverse f (Three e e' a) = fmap (Three e e') (f a)

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


-- 7
data Big a b = Big a b b

instance (Monoid a, Monoid b) => Monoid (Big a b) where
  -- mempty :: Big a b
  mempty = Big mempty mempty mempty

  -- mappend :: Big a b -> Big a b -> Big a b
  Big a0 b0 b0' `mappend` Big a1 b1 b1' = 
    Big (a0 `mappend` a1) (b0 `mappend` b1) (b0' `mappend` b1')

instance Functor (Big e) where
  -- fmap :: (a -> b) -> Big e a -> Big e b
  fmap f (Big e a a') = Big e (f a) (f a')

instance Monoid e => Applicative (Big e) where
  -- pure :: a -> Big e a
  pure a = Big mempty a a

  -- (<*>) :: Big e (a -> b) -> Big e a -> Big e b
  Big e0 f f' <*> Big e1 a a' = Big e0 (f a) (f' a')

instance Foldable (Big e) where
  -- foldMap :: Monoid m => (a -> m) -> Big e a -> m
  foldMap f (Big e a a') = f a `mappend` f a'

  -- foldr :: (a -> b -> b) -> b -> Big e a -> b
  foldr f b (Big e a a') = f a (f a b)

instance Traversable (Big e) where
  -- traverse :: Applicative f => (a -> f b) -> Big e a -> f (Big e b)
  traverse f (Big e a a') = pure (Big e) <*> (f a) <*> (f a')


-- 8
data Bigger a b = Bigger a b b b

instance Functor (Bigger e) where
  -- fmap :: (a -> b) -> Bigger e a -> Bigger e b
  fmap f (Bigger e a a' a'') = Bigger e (f a) (f a') (f a'')

instance Foldable (Bigger e) where
  -- foldMap :: Monoid m => (a -> m) -> Bigger e a -> m
  foldMap f (Bigger e a a' a'') = f a `mappend` f a' `mappend` f a''

  -- foldr :: (a -> b -> b) -> b -> Big e a -> b
  foldr f b (Bigger e a a' a'') = f a (f a' (f a'' b))
  
instance Traversable (Bigger e) where
  -- traverse :: Applicative f => (a -> f b) -> Bigger e a -> f (Bigger e b)
  traverse f (Bigger e a a' a'') =
    pure (Bigger e) <*> f a <*> f a' <*> f a''


-- 9
-- SkiFree

data S n a = S (n a) a deriving (Eq, Show)

instance ( Functor n
         , Arbitrary (n a)
         , Arbitrary a
         ) => Arbitrary (S n a) where
    arbitrary = S <$> arbitrary <*> arbitrary

instance ( Applicative n
         , Testable (n Property)
         , EqProp a
         ) => EqProp (S n a) where
    (S x y) =-= (S p q) = (property $ (=-=) <$> x <*> p) .&. (y =-= q)

-- instance Traversable n => Traversable (S n) where
  -- traverse :: Applicative f => (a -> f b) -> S n a -> f (S n b)

testS = sample' (arbitrary :: Gen (S [] Int))


-- 10
data Tree a =
    Empty
  | Leaf a
  | Node (Tree a) a (Tree a)
  deriving (Eq, Show)

instance Functor Tree where
  -- fmap :: (a -> b) -> Tree a -> Tree b
  fmap f Empty = Empty
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Node ta a ta') = Node (fmap f ta) (f a) (fmap f ta')

instance Foldable Tree where
  -- foldMap :: Monoid m => (a -> m) -> Tree a -> m
  foldMap f Empty = mempty
  foldMap f (Leaf a) = f a
  foldMap f (Node ta a ta') = 
    foldMap f ta `mappend` f a `mappend` foldMap f ta'
 
  -- foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr f b Empty = b
  foldr f b (Leaf a) = f a b
  foldr f b (Node ta a ta') = f a (foldr f (foldr f b ta') ta) 
  
instance Traversable Tree where
  -- traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
  traverse f Empty = pure Empty
  traverse f (Leaf a) = fmap Leaf (f a)
  traverse f (Node ta a ta') = 
    pure Node <*> (traverse f ta) <*> f a <*> (traverse f ta')
