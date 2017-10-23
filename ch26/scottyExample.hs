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
import Web.Scotty.Internal.Types
  (ActionT(..))
import Data.Monoid (mconcat)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Lazy
  hiding (get)
import Control.Monad

main :: IO ()
main = scotty 4000 $ do
  get "/:word" $ do
    beam <- param "word"
    (ActionT 
      . (ExceptT . fmap Right) 
      . ReaderT . const 
      . \m -> StateT (\s -> do
                        a <- m
                        return (a, s))
      ) (putStrLn "hello")
    html $
      mconcat ["<h1>Scottty, ", beam, "me up!</h1>"]

liftReaderT :: m a -> ReaderT r m a
liftReaderT m = ReaderT (const m)
