#!/usr/local/bin/stack
{- stack 
   exec ghci
   --resolver lts-9.3 
   --install-ghc 
   --package trifecta
   --package hspec
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.Monoid (mempty)
import Data.ByteString (ByteString)
import Control.Applicative 
  ( (*>)
  , (<*)
  , (<|>)
  )
import Text.Trifecta 
  ( Parser
  , Result (Success)
  , char
  , some
  , letter
  , oneOf
  , noneOf
  , skipMany
  , parseByteString
  , parseString
  )
import Data.Map (Map)
import qualified Data.Map as M
import Text.RawString.QQ (r)
import Test.Hspec
  ( hspec
  , describe
  , it
  , shouldBe
  )

headerEx :: ByteString
headerEx = "[blah]"

newtype Header = Header String
  deriving (Eq, Ord, Show)

parseBracketPair :: Parser a -> Parser a
parseBracketPair p = char '[' *> p <* char ']'

parseHeader :: Parser Header
parseHeader = parseBracketPair (Header <$> some letter)

assignmentEx :: ByteString
assignmentEx = "woot=1"

type Name = String
type Value = String
type Assignments = Map Name Value

parseAssignment :: Parser (Name, Value)
parseAssignment =
  some letter >>= \name ->
    char '=' >>
      some (noneOf ['\n']) >>= \value ->
        skipEOL >>
          return (name, value)

skipEOL :: Parser ()
skipEOL = skipMany (oneOf ['\n'])

commentEx :: ByteString
commentEx = "; last modified 1 April\\ 2001 by John Doe"

commentEx' :: ByteString
commentEx' = "; blah\n; woot\n  \n; hah"

skipComments :: Parser ()
skipComments =
  skipMany ((char ';' <|> char '#') >>
              skipMany (noneOf ['\n']) >>
                skipEOL)
              
sectionEx :: ByteString
sectionEx = "; ignore me\n[states]\nChris=Texas"

sectionEx' :: ByteString
sectionEx' = [r|
; ignore me
[states]
Chris=Texas
|]

sectionEx'' :: ByteString
sectionEx'' = [r|
; comment
[section]
host=wikipedia.org
alias=claw

[whatisit]
red=intoothandclaw
|]

data Section = Section Header Assignments
  deriving (Eq, Show)

newtype Config = Config (Map Header Assignments)
  deriving (Eq, Show)

skipWhitespace :: Parser ()
skipWhitespace = skipMany (char ' ' <|> char '\n')

parseSection :: Parser Section
parseSection = do
  skipWhitespace
  skipComments
  header <- parseHeader
  skipEOL
  assignments <- some parseAssignment
  return $
    Section header (M.fromList assignments)

rollup :: Section -> Map Header Assignments -> Map Header Assignments
rollup (Section header assignments) map =
  M.insert header assignments map

parseIni :: Parser Config
parseIni = do
  sections <- some parseSection

  let mapOfSections =
        foldr rollup M.empty sections
  return (Config mapOfSections)

maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _ = Nothing

main :: IO ()
main = hspec $ do
  describe "Assignment parsing" $
    it "can parse a simple assignment" $ do
      let m = parseByteString
              parseAssignment
              mempty assignmentEx
          r' = maybeSuccess m
      print m
      r' `shouldBe` Just ("woot", "1")

  describe "Header parsing" $
    it "can parse a simple header" $ do
      let m = parseByteString
              parseHeader
              mempty headerEx
          r' = maybeSuccess m
      print m
      r' `shouldBe` Just (Header "blah")

  describe "Comment parsing" $
    it "can parse a simple section" $ do
      let m = parseByteString
              parseSection
              mempty sectionEx
          r' = maybeSuccess m
          states =
            M.fromList [("Chris", "Texas")]
          expected' =
            Just (Section (Header "states") states)
      print m
      r' `shouldBe` expected'

  describe "INI parsing" $
    it "can parse multiple sections" $ do
      let m = parseByteString
              parseIni
              mempty sectionEx''
          r' = maybeSuccess m
          sectionValues =
            M.fromList
            [ ("alias", "claw")
            , ("host", "wikipedia.org")
            ]
          whatisitValues =
            M.fromList
            [("red", "intoothandclaw")]
          expected' =
            Just (Config
                    (M.fromList
                      [ (Header "section"
                      , sectionValues)
                      , (Header "whatisit"
                      , whatisitValues)
                      ]))
      print m
      r' `shouldBe` expected'


