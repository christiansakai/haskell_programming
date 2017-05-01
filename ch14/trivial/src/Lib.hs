{-# LANGUAGE DeriveGeneric #-}

module Lib where

import Test.QuickCheck
import Test.QuickCheck.Gen ( oneof )
import GHC.Generics

trivialPlay :: IO ()
trivialPlay = do
  sample trivialGen

data Trivial = Trivial deriving (Eq, Show)

trivialGen :: Gen Trivial
trivialGen =
  return Trivial

instance Arbitrary Trivial where
  arbitrary = trivialGen


identityPlay :: IO ()
identityPlay = do
  sample identityGenInt

data Identity a = Identity a deriving (Eq, Show)

identityGen :: Arbitrary a => Gen (Identity a)
identityGen = do
  a <- arbitrary
  return (Identity a)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = identityGen

identityGenInt :: Gen (Identity Int)
identityGenInt = identityGen


data Pair a b = Pair a b deriving (Eq, Show)

pairGen :: ( Arbitrary a
           , Arbitrary b) => Gen (Pair a b)
pairGen = do
  a <- arbitrary
  b <- arbitrary
  return (Pair a b)

instance ( Arbitrary a
         , Arbitrary b) =>
           Arbitrary (Pair a b) where
  arbitrary = pairGen

pairGenIntString :: Gen (Pair Int String)
pairGenIntString = pairGen

pairPlay :: IO ()
pairPlay = do
  sample pairGenIntString


data Sum a b = First a
             | Second b
             deriving ( Eq, Show )

-- equal odds for each
sumGenEqual :: ( Arbitrary a
               , Arbitrary b) =>
                 Gen (Sum a b)
sumGenEqual = do
  a <- arbitrary
  b <- arbitrary
  oneof [ return $ First a
        , return $ Second b ]

sumGenCharInt :: Gen (Sum Char Int)
sumGenCharInt = sumGenEqual

sumEqualPlay :: IO ()
sumEqualPlay = do
  sample pairGenIntString


sumGenFirstPls :: ( Arbitrary a
                  , Arbitrary b ) =>
                    Gen (Sum a b)
sumGenFirstPls = do
  a <- arbitrary
  b <- arbitrary
  frequency [ (10, return $ First a)
            , (1, return $ Second b) ]

sumGenCharIntFirst :: Gen (Sum Char Int)
sumGenCharIntFirst = sumGenFirstPls

sumFirstPlay :: IO ()
sumFirstPlay = do
  sample sumGenCharIntFirst


data Bool'
  = True'
  | False'
  deriving ( Generic )

instance CoArbitrary Bool'

trueGen :: Gen Int
trueGen =
  coarbitrary True' arbitrary

truePlay :: IO ()
truePlay = 
  sample trueGen

falseGen :: Gen Int
falseGen =
  coarbitrary False' arbitrary

falsePlay :: IO ()
falsePlay =
  sample falseGen
