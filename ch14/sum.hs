module Main where

import Test.QuickCheck

data Sum a b = First a
             | Second b
             deriving (Eq, Show)

sumGenEqual :: (Arbitrary a, Arbitrary b)
               => Gen (Sum a b)
sumGenEqual = do
  a <- arbitrary
  b <- arbitrary
  oneof [return $ First a, return $ Second b]

sumGenCharInt :: Gen (Sum Char Int)
sumGenCharInt = sumGenEqual

sumGenFirstPls :: (Arbitrary a, Arbitrary b)
                  => Gen (Sum a b)
sumGenFirstPls = do
  a <- arbitrary
  b <- arbitrary
  frequency [(10, return $ First a),
             (1, return $ Second b)]

sumGenCharIntFirst :: Gen (Sum Char Int)
sumGenCharIntFirst = sumGenFirstPls

main :: IO ()
main = do
  sample sumGenCharInt

  sample sumGenCharIntFirst
