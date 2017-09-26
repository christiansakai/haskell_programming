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

badFraction = "1/0"
alsoBad = "10"
shouldWork = "1/2"
shouldAlsoWork = "2/1"

parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  return (numerator % denominator)

virtuousFraction :: Parser Rational
virtuousFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "Denominator cannot be zero"
    _ -> return (numerator % denominator)

main :: IO ()
main = do
  let parseFraction' = 
        parseString parseFraction mempty

  print $ parseFraction' shouldWork
  print $ parseFraction' shouldAlsoWork
  print $ parseFraction' alsoBad
  print $ parseFraction' badFraction

main' :: IO ()
main' = do
  let virtuousFraction' = 
        parseString virtuousFraction mempty

  print $ virtuousFraction' badFraction
  print $ virtuousFraction' alsoBad
  print $ virtuousFraction' shouldWork
  print $ virtuousFraction' shouldAlsoWork








