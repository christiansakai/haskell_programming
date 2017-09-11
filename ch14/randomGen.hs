#!/usr/local/bin/stack
{- stack 
   exec ghci
   --resolver lts-9.3 
   --install-ghc 
   --package QuickCheck
   --package hspec
-}

import Test.QuickCheck

-- 1
data Fool = Fulse
          | Frue
          deriving (Eq, Show)

instance Arbitrary Fool where
  -- arbitrary :: Gen Fool
  arbitrary = frequency [ (1, return Fulse)
                        , (1, return Frue)
                        ]

-- 2
data Fool' = Fulse'
           | Frue'
           deriving (Eq, Show)

instance Arbitrary Fool' where
  -- arbitrary :: Gen Fool'
  arbitrary = frequency [ (2, return Fulse')
                        , (3, return Frue')
                        ]
