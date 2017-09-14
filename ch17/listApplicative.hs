#!/usr/local/bin/stack
{- stack 
   exec ghci
   --resolver lts-9.3 
   --install-ghc 
   --package QuickCheck
   --package hspec
-}

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

main :: IO ()
main = 
  print "Hello"

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys =
  Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f la = concat' $ fmap f la

data List a = 
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  -- fmap :: (a -> b) -> List a -> List b
  fmap _ Nil = Nil
  fmap f (Cons a la) = Cons (f a) (fmap f la)

instance Applicative List where
  -- pure :: a -> List a 
  pure a = Cons a Nil

  -- (<*>) :: List (a -> b) -> List a -> List b
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  (Cons f lf) <*> la  = 
    fmap f la `append` (lf <*> la)

instance (Eq a, EqProp a) => EqProp (List a) where
  -- (=-=) :: List a -> List a -> Property
  (=-=) = eq
