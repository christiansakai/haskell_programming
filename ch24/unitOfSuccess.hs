#!/usr/local/bin/stack
{- stack 
   exec ghci
   --resolver lts-9.3 
   --install-ghc 
   --package trifecta
-}

import Text.Trifecta
  ( parseString
  , integer
  , eof
  )
import Text.Parser.Token (TokenParsing)
import Data.Semigroup (mempty)
import Data.Ratio ((%))

yourFuncHere :: (Monad m, TokenParsing m) => m Integer
yourFuncHere =
  integer >>= \int ->
    eof >>
      return int

main :: IO ()
main = do
  print $ parseString (yourFuncHere) mempty "123"
  print $ parseString (yourFuncHere) mempty "123abc"
