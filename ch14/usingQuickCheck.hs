#!/usr/local/bin/stack
{- stack 
   exec ghci
   --resolver lts-9.3 
   --install-ghc 
   --package QuickCheck
   --package hspec
-}

import Prelude hiding (quotRem, divMod)
import Data.List (sort)
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = do
  quickCheck prop_half
  quickCheck prop_listOrdered
  quickCheck prop_plusAssoc
  quickCheck prop_plusComm
  quickCheck prop_mulAssoc
  quickCheck prop_mulComm
  quickCheck prop_quotRem
  quickCheck prop_divMod
  quickCheck prop_doubleReverseList
  quickCheck prop_dollarSign
  quickCheck prop_foldrPlusList
  quickCheck prop_f


-- 1
half :: Fractional a => a -> a
half x = x / 2

halfIdentity :: Fractional a => a -> a
halfIdentity = (*2) . half

prop_half :: Double -> Bool
prop_half x = x == halfIdentity x

-- 2
listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = 
  snd $ foldr go (Nothing, True) xs
    where go _ status@(_, False) = status
          go y (Nothing, t) = (Just y, t)
          go y (Just x, t) = (Just y, x >= y)

prop_listOrdered :: [Int] -> Bool
prop_listOrdered as = listOrdered $ sort as

-- 3
plusAssociative :: (Num a, Eq a) => a -> a -> a -> Bool
plusAssociative x y z =
  x + (y + z) == (x + y) + z

plusCommutative :: (Num a, Eq a) => a -> a -> Bool
plusCommutative x y = x + y == y + x

prop_plusAssoc :: Int -> Int -> Int -> Bool
prop_plusAssoc = plusAssociative

prop_plusComm :: Int -> Int -> Bool
prop_plusComm = plusCommutative

-- 4
mulAssociative :: (Num a, Eq a) => a -> a -> a -> Bool
mulAssociative x y z =
  x * (y * z) == (x* y) * z

mulCommutative :: (Num a, Eq a) => a -> a -> Bool
mulCommutative x y = x * y == y * x

prop_mulAssoc :: Int -> Int -> Int -> Bool
prop_mulAssoc = mulAssociative

prop_mulComm :: Int -> Int -> Bool
prop_mulComm = mulCommutative

-- 5
quotRem :: Integral a => a -> NonZero a -> Bool
quotRem x (NonZero y) = (quot x y) * y + (rem x y) == x

divMod :: Integral a => a -> NonZero a -> Bool
divMod x (NonZero y) = (div x y) * y + (mod x y) == x

prop_quotRem :: Int -> NonZero Int -> Bool
prop_quotRem = quotRem

prop_divMod :: Int -> NonZero Int -> Bool
prop_divMod = divMod

-- 6
-- Power is not 100% associative or 100% commutative

-- 7
prop_doubleReverseList :: [Int] -> Bool
prop_doubleReverseList as = (reverse . reverse) as == id as

-- 8
prop_dollarSign :: Int -> Bool
prop_dollarSign a = id $ a == id a

prop_compose :: Int -> Bool
prop_compose a = ((+1) . (*2)) a == (+1) ((*2) a)

-- 9
-- foldr (:) is not the same as (++)

prop_foldrPlusList :: [[Int]] -> Bool
prop_foldrPlusList as = foldr (++) [] as == concat as

-- 10
-- Not true
-- f n xs = length (take n xs) == n

-- 11
-- Not true
-- f x = ((read (show x)) :: String) == "x"

