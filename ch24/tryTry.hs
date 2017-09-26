#!/usr/local/bin/stack
{- stack 
   exec ghci
   --resolver lts-9.3 
   --install-ghc 
   --package trifecta
-}

{-# LANGUAGE OverloadedStrings #-}

import Text.Trifecta
import Text.Parser.Token 
  ( decimal
  , TokenParsing
  )
import Data.Ratio ((%))
import Control.Applicative ((<|>))

parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "Denominator cannot be zero"
    _ -> return (numerator % denominator)

type DecimalOrFraction = Either Rational Integer

parseDecimalOrFraction :: Parser DecimalOrFraction
parseDecimalOrFraction =
  try (Left <$> parseFraction) <|> (Right <$> decimal)

main :: IO ()
main = do
  let parseDecimalOrFraction' = 
        parseString parseDecimalOrFraction mempty

  print $ parseDecimalOrFraction' "1"
  print $ parseDecimalOrFraction' "1/2"

