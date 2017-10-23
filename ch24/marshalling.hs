#!/usr/local/bin/stack
{- stack 
   exec ghci
   --resolver lts-9.3 
   --install-ghc 
   --package aeson
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import Data.Aeson 
  ( Value (Object, Number, String)
  , FromJSON
  , parseJSON
  , decodeStrict
  , encode
  , decode
  , eitherDecode
  , (.:)
  )
import Data.Text (Text)
import Text.RawString.QQ (r)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Control.Applicative ((<|>))
import Data.Scientific (floatingOrInteger)

sectionJson :: LBS.ByteString
sectionJson = [r|
{ "section": {"host": "wikipedia.org"},
  "whatisit": {"red": "intoothandclaw"}
}
|]

data TestData = 
  TestData {
    section :: Host
  , what :: Color
  } deriving (Eq, Show)

newtype Host = Host String 
  deriving (Eq, Show)

type Annotation = String

data Color = 
    Red Annotation
  | Blue Annotation
  | Yellow Annotation
  deriving (Eq, Show)

instance FromJSON TestData where
  -- parseJSON :: Value -> Parser TestData
  parseJSON (Object v) = 
    TestData <$> v .: "section"
             <*> v .: "whatisit"
  parseJSON _ = fail "Expected an object for TestData"

instance FromJSON Host where
  -- parseJSON :: Value -> Parser Host
  parseJSON (Object v) = 
    Host <$> v .: "host"
  parseJSON _ = fail "Expected an object for Host"

instance FromJSON Color where
  -- parseJSON :: Value -> Parser Color
  parseJSON (Object v) = 
        (Red <$> v .: "red")
    <|> (Blue <$> v.: "blue")
    <|> (Yellow <$> v.: "yellow")
  parseJSON _ = fail "Expected an object for Color"


main :: IO ()
main = do
  let d :: Maybe TestData
      d = decode sectionJson
  print d

data NumberOrString =
    Numba Integer
  | Stringy Text
  deriving (Eq, Show)

instance FromJSON NumberOrString where
  parseJSON (Number i) = 
    case floatingOrInteger i of
      Left _ -> fail "Must be integral number"
      Right integer -> return $ Numba integer
  parseJSON (String s) = return $ Stringy s
  parseJSON _ = 
    fail "NumberOrString must\
          \ be number or string"

dec :: LBS.ByteString -> Maybe NumberOrString
dec = decode

eitherDec :: LBS.ByteString -> Either String NumberOrString
eitherDec = eitherDecode

main' :: IO ()
main' = do
  print $ dec "blah"
  print $ eitherDec "blah"
