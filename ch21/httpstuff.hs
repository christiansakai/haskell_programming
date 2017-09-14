#!/usr/local/bin/stack
{- stack 
   exec ghci
   --resolver lts-9.3 
   --install-ghc 
   --package wreq
-}

import Data.ByteString.Lazy hiding (map)
import Network.Wreq

urls :: [String]
urls = [ "http://httpbin.org/ip"
       , "http://httpbin.org/bytes/5"
       ]

mappingGet :: [IO (Response ByteString)]
mappingGet = map get urls

traversedUrls :: IO [Response ByteString]
traversedUrls = traverse get urls
