module Main where

import Lib

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

main :: IO ()
main = print "Hello"

data List a = 
    Nil 
  | Cons a (List a)
  deriving (Eq, Show)

take' :: Int -> List a -> List a
take' 0 la = Nil
take' n (Cons a la) = Cons a (take' (n - 1) la)

repeat' :: a -> List a
repeat' n = Cons n $ repeat' n

instance Functor List where
  -- fmap :: (a -> b) -> List a -> List b
  fmap f Nil = Nil
  fmap f (Cons a la) = Cons (f a) (fmap f la)

instance Applicative List where
  -- pure :: a -> List a
  pure a = Cons a Nil

  -- (<*>) :: f (a -> b) -> f a -> f b
  Nil <*> la = Nil
  lf <*> Nil = Nil
  (Cons f lf) <*> (Cons a la) = Cons (f a) (lf <*> la)

newtype ZipList' a =
  ZipList' (List a)
  deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  -- (=-=)
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
                 in take' 3000 l
          ys' = let (ZipList' l) = ys
                 in take' 3000 l

instance Functor ZipList' where
  -- fmap :: (a -> b) -> List a -> List b
  fmap f (ZipList' xs) = 
    ZipList' $ fmap f xs

instance Applicative ZipList' where
  -- pure :: a -> ZipList' a
  pure a = ZipList' (Cons a Nil)

  -- (<*>) :: f (a -> b) -> f a -> f b
  ZipList' la <*> ZipList' lb = ZipList' $ la <*> lb


zf = ZipList' (Cons (+9) (Cons (*2) (Cons (+8) Nil)))
za = ZipList' (Cons 1 (Cons 2 (Cons 3 Nil)))
zr = zf <*> za

zb = ZipList' (repeat' 1)
zr' = zf <*> zb
