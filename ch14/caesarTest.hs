#!/usr/local/bin/stack
{- stack 
   exec ghci
   --resolver lts-9.3 
   --install-ghc 
   --package hspec
-}

import Test.Hspec
import Test.QuickCheck
import Data.Char (ord, chr, isLetter, toLower)

main :: IO ()
main = do
  quickCheck prop_encodeDecode

prop_encodeDecode :: Int -> Bool
prop_encodeDecode n = 
  let word = "haskell is awesome"
   in word == (uncaesar (caesar word n) n)

caesar :: String -> Int -> String
caesar ""     n  = ""
caesar (x:xs) n 
  | isLetter x = encode x n : caesar xs n
  | otherwise  = x : caesar xs n

encode :: Char -> Int -> Char
encode c n = chr $ (ord c + n - ord 'a' ) `mod` 26  + ord 'a'

decode :: Char -> Int -> Char
decode c n = chr $ (ord c - n - ord 'a') `mod` 26 + ord 'a'

uncaesar :: String -> Int -> String
uncaesar "" n     = ""
uncaesar (x:xs) n 
  | isLetter x = decode x n : uncaesar xs n
  | otherwise  = x : uncaesar xs n
