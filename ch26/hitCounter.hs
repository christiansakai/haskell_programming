{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Data.IORef
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import System.Environment (getArgs)
import Web.Scotty.Trans

data Config =
  Config {
    counts :: IORef (M.Map Text Integer)
  , prefix :: Text
  }

type Scotty =
  ScottyT Text (ReaderT Config IO)

type Handler =
  ActionT Text (ReaderT Config IO)

bumpBoomp :: Text
          -> M.Map Text Integer
          -> (M.Map Text Integer, Integer)
bumpBoomp k m = 
  case M.lookup k m of
    Just val -> (m, val)
    Nothing -> (m, 0)

app :: Scotty ()
app = 
  get "/:key" $ do
    config <- lift ask :: Handler Config
    -- can also be
    -- thePrefix <- lift $ asks prefix :: Handler Text
    
    unprefixed <- param "key" :: Handler Text
          
    let key' = mappend (prefix config) unprefixed :: Text

        _ = readIORef $ counts config :: IO (M.Map Text Integer)

    store <- liftIO $ readIORef $ counts config

    -- not sure this is correct or not
    let newInteger = bumpBoomp key' store
    html $
      mconcat [ "<h1>Success! Count was: "
              , TL.pack $ show newInteger
              , "</h1>"
              ]

main :: IO ()
main = do
  [prefixArg] <- getArgs
  counter <- newIORef M.empty
  let config :: Config
      config = Config counter "alpha"

      runR = undefined
  scottyT 3000 runR app
    

data A = A
