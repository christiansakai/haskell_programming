#!/usr/local/bin/stack
{- stack 
   exec ghci
   --resolver lts-9.3 
   --install-ghc 
   --package QuickCheck
   --package hspec
-}

import Test.QuickCheck
import Data.Char (toUpper)
import Data.List (sort)

main :: IO ()
main = do
  quickCheck prop_f1
  quickCheck prop_f2

twice :: (a -> a) -> (a -> a)
twice f = f . f

fourTimes :: (a -> a) -> (a -> a)
fourTimes = twice . twice

capitalizeWord :: String -> String
capitalizeWord "" = ""
capitalizeWord (a:as) = toUpper a : capitalizeWord as

prop_f1 :: String -> Bool
prop_f1 xs = 
  (capitalizeWord xs == twice capitalizeWord xs) &&
  (capitalizeWord xs == fourTimes capitalizeWord xs)

prop_f2 :: [Int] -> Bool
prop_f2 xs = 
  (sort xs == twice sort xs) &&
  (sort xs == fourTimes sort xs)

