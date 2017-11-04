#!/usr/local/bin/stack
{- stack 
   exec ghci
   --resolver lts-9.3 
   --install-ghc 
   --package scotty
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Web.Scotty
import Control.Monad.IO.Class
import Data.Monoid (mconcat)

main :: IO ()
main = scotty 3000 $ doScotty

doScotty :: ScottyM ()
doScotty =
  get ":/word" $ do
    beam <- param "word"
    liftIO $ putStrLn "hello"
    html $
      mconcat [ "<h1>Scotty, "
              , beam
              , " me up!</h1>"
              ]



