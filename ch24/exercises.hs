#!/usr/local/bin/stack
{- stack 
   exec ghci
   --resolver lts-9.3 
   --install-ghc 
   --package aeson
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import Text.Trifecta
  ( Parser
  , Result
  , integer
  , char
  , letter
  , parseString
  , stringLiteral
  , try
  , some
  , many
  , sepBy
  , option
  , oneOf
  , optional
  , count
  , digit
  , choice
  , space
  , symbol
  , between
  , string
  , anyChar
  , skipMany
  , newline
  , whiteSpace
  , notChar
  , alphaNum
  , spaces
  )
import Control.Monad ((>>))
import Control.Applicative ((<|>))
import Data.Char (digitToInt)
import Text.RawString.QQ (r)
import Data.Map (Map)
import qualified Data.Map as M

-- 1

data NumberOrString =
    NOSS String
  | NOSI Integer
  deriving (Eq, Show, Ord)

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer =
  SemVer Major Minor Patch Release Metadata
  deriving (Eq, Show)

instance Ord SemVer where
  -- compare :: SemVer -> Semver -> Ordering
  compare sem1 sem2 = 
    let (SemVer maj1 min1 pat1 rel1 met1) = sem1
        (SemVer maj2 min2 pat2 rel2 met2) = sem2
     in case compare maj1 maj2 of
          LT -> LT
          GT -> GT
          _ -> 
            case compare min1 min2 of
              LT -> LT
              GT -> GT
              _ ->
                case compare pat1 pat2 of
                  LT -> LT
                  GT -> GT
                  _ -> 
                    case compare rel1 rel2 of
                      LT -> LT
                      GT -> GT
                      _ -> 
                        case compare met1 met2 of
                          LT -> LT
                          GT -> GT
                          _  -> EQ

      
parseSemver :: Parser SemVer
parseSemver = do
  major <- integer
  char '.'

  minor <- integer
  char '.'

  patch <- integer

  release <- option [] (do
    char '-'
    sepBy parseNOS (char '.'))

  return $ SemVer major minor patch release []
  -- undefined

parseNOS :: Parser NumberOrString
parseNOS = try (NOSI <$> integer) <|> (NOSS <$> (some letter))

no1 :: IO ()
no1 = do
  print $ parseString parseSemver mempty "2.1.1"
  print $ parseString parseSemver mempty "1.0.0-x.7.z.92"


-- 2

parseDigit :: Parser Char
parseDigit = oneOf "1234567890"

base10Integer :: Parser Integer
base10Integer = 
  some parseDigit >>= \digitString ->
    return $ read digitString


no2 :: IO ()
no2 = do
  print $ parseString parseDigit mempty "123"
  print $ parseString parseDigit mempty "abc"
  print $ parseString base10Integer mempty "123abc"
  print $ parseString base10Integer mempty "abc"


-- 3

base10Integer' :: Parser Integer
base10Integer' = 
  optional (char '-') >>= \res ->
    case res of
      Just _ -> 
        negate <$> base10Integer
      Nothing ->
        base10Integer
  
no3 :: IO ()
no3 = do
  print $ parseString base10Integer' mempty "-123abc"

-- 4

type NumberingPlanArea = Int
type Exchange = Int
type LineNumber = Int

data PhoneNumber =
  PhoneNumber NumberingPlanArea
              Exchange
              LineNumber
  deriving (Eq, Show)

parse3Digit :: Parser Int
parse3Digit = 
  digit >>= \d1 ->
    digit >>= \d2 ->
      digit >>= \d3 ->
        return $ (digitToInt d1 :: Int) * 100 
               + (digitToInt d2 :: Int) * 10 
               + (digitToInt d3 :: Int) * 1

parse4Digit :: Parser Int
parse4Digit = 
  digit >>= \d1 ->
    digit >>= \d2 ->
      digit >>= \d3 ->
        digit >>= \d4 ->
          return $ (digitToInt d1 :: Int) * 1000
                 + (digitToInt d2 :: Int) * 100
                 + (digitToInt d3 :: Int) * 10
                 + (digitToInt d4 :: Int) * 1

parsePhone' :: Parser PhoneNumber
parsePhone' = do
  npa <- parse3Digit
  optional (char '-')
  exc <- parse3Digit
  optional (char '-')
  line <- parse4Digit
  return $ PhoneNumber npa exc line

parsePhone'' :: Parser PhoneNumber
parsePhone'' = do
  npa <- between (symbol "(") (symbol ")") parse3Digit
  exc <- parse3Digit
  optional (char '-')
  line <- parse4Digit
  return $ PhoneNumber npa exc line

parsePhone''' :: Parser PhoneNumber
parsePhone''' = do
  digit
  char '-'
  parsePhone'

parsePhone :: Parser PhoneNumber
parsePhone = 
  try parsePhone'
  <|> parsePhone''
  <|> parsePhone'''

no4 :: IO ()
no4 = do
  print $ parseString parsePhone mempty "123-456-7890"
  print $ parseString parsePhone mempty "1234567890"
  print $ parseString parsePhone mempty "(123) 456-7890"
  print $ parseString parsePhone mempty "1-123-456-7890"


-- 5

logTestData :: String
logTestData = [r|
-- whee a comment

# 2025-02-05
08:00 Breakfast
09:00 Sanitizing moisture collector
11:00 Exercising in high-grave gym
12:00 Lunch
13:00 Programming
17:00 Commuting home in rover
17:30 R&R
19:00 Dinner
21:00 Shower
21:15 Read
22:00 Sleep

# 2025-02-07 -- dates not necessarily sequential
08:00 Breakfast -- should I try skippin breakfast?
09:00 Bumped head, passed out
13:36 Wake up, headache
13:37 Go to medbay
13:40 Patch self up
13:45 Commute home for rest
14:15 Read
21:00 Dinner
21:14 Read
22:00 Sleep
|]

type Hour = String
type Event = String
type Date = String

data Hourly = Hourly Hour Event

data Daily = Daily Date Hourly

-- data LogData = [Daily]

parseComment :: Parser Char
parseComment = do
  newline
  string "--"
  whiteSpace
  many (alphaNum <|> spaces)
  newline
  return ()

no5 :: IO ()
no5 =
  print $ parseString parseComment mempty logTestData


