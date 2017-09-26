#!/usr/local/bin/stack
{- stack 
   exec ghci
   --resolver lts-9.3 
   --install-ghc 
   --package trifecta
   --package raw-strings-qq
-}

{-# LANGUAGE QuasiQuotes #-}

import Text.Trifecta
import Control.Applicative
import Text.RawString.QQ

type NumberOrString = Either Integer String

a = "blah"
b = "123"
c = "123blah789"

parseNOS :: Parser NumberOrString
parseNOS = do
  skipMany (oneOf "\n")
  v <- (Left <$> integer) <|> (Right <$> some letter)
  skipMany (oneOf "\n")
  return v

eitherOr :: String
eitherOr = [r|
123
abc
456
def|]

main = do
  let p f i = 
        parseString f mempty i

  print $ p (some letter) a
  print $ p integer b
  print $ p parseNOS a
  print $ p parseNOS b
  print $ p (many parseNOS) c
  print $ p (some parseNOS) c

main' = do
  let p f i = parseString f mempty i
  print $ p parseNOS eitherOr
