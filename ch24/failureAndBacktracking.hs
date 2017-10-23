#!/usr/local/bin/stack
{- stack 
   exec ghci
   --resolver lts-9.3 
   --install-ghc 
   --package trifecta
   --package attoparsec
-}

{-# LANGUAGE OverloadedStrings #-}

import Control.Monad ((>>))
import Control.Applicative ((<|>))
import Text.Trifecta 
  ( Parser
  , CharParsing
  , parseString
  , char
  , try
  )
import Text.Parser.Combinators ((<?>))
import Text.Parsec (Parsec, parseTest)
import qualified Data.Attoparsec.ByteString as A
import Data.Attoparsec.ByteString (parseOnly)
import Data.ByteString (ByteString)

-- Helper functions for three different Parsers

trifP :: Show a => Parser a -> String -> IO ()
trifP p i = 
  print $ parseString p mempty i

parsecP :: Show a => Parsec String () a -> String -> IO ()
parsecP = parseTest

attoP :: Show a => A.Parser a -> ByteString -> IO ()
attoP p i = 
  print $ parseOnly p i

-- Parser functions

nobackParse :: (Monad f, CharParsing f) => f Char
nobackParse = (char '1' >> char '2') <|> char '3'

tryParse :: (Monad f, CharParsing f) => f Char
tryParse = try (char '1' >> char '2') <|> char '3'

main :: IO ()
main = do
  -- trifecta
  trifP nobackParse "13"
  trifP tryParse "13"

  -- parsec
  parsecP nobackParse "13"
  parsecP tryParse "13"

  -- attoparsec
  attoP nobackParse "13"
  attoP tryParse "13"

tryAnnot :: (Monad f, CharParsing f) => f Char
tryAnnot = (try (char '1' >> char '2') <?> "Tried 12") 
       <|> (char '3' <?> "Tried 3")

