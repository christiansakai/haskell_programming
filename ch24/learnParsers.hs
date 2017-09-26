#!/usr/local/bin/stack
{- stack 
   exec ghci
   --resolver lts-9.3 
   --install-ghc 
   --package trifecta
-}

import Text.Trifecta
import Control.Monad ((>>))
import Text.Parser.Combinators (eof)

stop :: Parser a
stop = unexpected "stop"

one :: CharParsing m => m Char
one = char '1'

one' :: Parser a
one' = one >> stop

oneTwo :: (Monad m, CharParsing m) => m Char
oneTwo = char '1' >> char '2'

oneTwo' :: Parser a
oneTwo' = oneTwo >> stop

testParse :: Parser Char -> IO ()
testParse p = 
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
