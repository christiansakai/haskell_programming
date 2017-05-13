import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data List a = 
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

take' :: Int -> List a -> List a
take' _ Nil         = Nil
take' 0 _           = Nil
take' n (Cons a la) = Cons a (take' (n - 1) la)

append :: List a -> List a -> List a
append Nil la         = la
append (Cons a la) lb = Cons a (la `append` lb)

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil          = b
fold f b (Cons a la)  = f a (fold f b la)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f la = concat' $ fmap f la

instance Functor List where
  -- fmap :: (a -> b) -> List a -> List b
  fmap _ Nil          = Nil
  fmap f (Cons a la)  = Cons (f a) (fmap f la)

instance Applicative List where
  -- pure :: a -> List a
  pure a = Cons a Nil

  -- (<*>) :: List (a -> b) -> List a -> List b
  Nil <*> _         = Nil
  _ <*> Nil         = Nil
  Cons f lf <*> la = 
    fmap f la `append` (lf <*> la)

newtype ZipList' a =
  ZipList' (List a)
  deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
                 in take' 3000 l
          ys' = let (ZipList' l) = ys
                 in take' 3000 l

instance Functor ZipList' where
  -- fmap :: (a -> b) -> ZipList' a -> Ziplist' b
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

-- instance Applicative ZipList' where
--   -- pure :: a -> ZipList' a
--   pure a = ZipList' (pure a)
    
--   -- (<*>) :: ZipList' (a -> b) -> ZipList' a -> ZipList' b
--   _ <*> ZipList' Nil  = ZipList' Nil
--   ZipList' Nil <*> _  = ZipList' Nil
--   ZipList' (Cons f lf) <*> ZipList' (Cons a la) = 
--      ZipList' (Cons (f a) (apply lf la))
--       where apply _ Nil = Nil
--             apply Nil _ = Nil
--             apply (Cons f lf) (Cons a la) = 
--               f a `append` apply lf la









-- --   -- (<*>) (ZipList' Nil) _ = ZipList' Nil
-- --   -- (<*>) _ (ZipList' Nil) = ZipList' Nil
-- --   -- (<*>) (ZipList' (Cons f lf)) (ZipList' (Cons a la)) = 
-- --   --   ZipList' (Cons (f a) ()




