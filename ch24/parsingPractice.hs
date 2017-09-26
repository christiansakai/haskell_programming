#!/usr/local/bin/stack
{- stack 
   exec ghci
   --resolver lts-9.3 
   --install-ghc 
   --package trifecta
-}

import Text.Trifecta
import Text.Parser.Combinators (eof)
import Control.Monad ((>>))

stop :: Parser a
stop = unexpected "stop"

-- 1
one :: (Monad m, CharParsing m) => m Char
one = 
  char '1' >>= \x ->
    eof >>
      return x

one' :: Parser a
one' = one >> stop

-- 1
oneTwo :: (Monad m, CharParsing m) => m Char
oneTwo = do
  char '1' 
  x <- char '2'
  eof
  return x

oneTwo' :: Parser a
oneTwo' = oneTwo >> stop

-- 2
oneTwoThree :: Parser String
oneTwoThree = choice 
  [ string "1"
  , string "12"
  , string "123" >> stop
  ]

oneTwoThree' :: Parser Char
oneTwoThree' = choice
  [ char '1'
  , char '1' >> char '2'
  , char '1' >> char '2' >> char '3' >> stop
  ]

testParse :: Parser Char -> IO ()
testParse p = 
  print $ (parseString p mempty "123")

-- 2
testParse' :: Parser String -> IO ()
testParse' p = 
  print $ (parseString p mempty "123")

pNL :: String -> IO ()
pNL s = 
  putStrLn ('\n' : s)
  
main :: IO ()
main = do
  pNL "stop:"
  testParse stop

  pNL "one:"
  testParse one

  pNL "one':"
  testParse one'

  pNL "oneTwo:"
  testParse oneTwo

  pNL "oneTwo':"
  testParse oneTwo'

  pNL "oneTwoThree:"
  testParse' oneTwoThree

  pNL "oneTwoThree:"
  testParse oneTwoThree'

