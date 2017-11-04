module Main where

import Control.Monad (forever)
import Network.Socket hiding (recv)
import Network.Socket.ByteString
  (recv, sendAll)

logAndEcho :: Socket -> IO ()
logAndEcho socket = forever $ do
  (socket', _) <- accept socket
  printAndKickback socket'
  close socket'
    where
      printAndKickback conn = do
        msg <- recv conn 1024
        print msg
        sendAll conn msg

main :: IO ()
main = withSocketsDo $ do
  addrinfos <- getAddrInfo
               (Just (defaultHints
                 {addrFlags =
                   [AI_PASSIVE]}))
               Nothing (Just "79")
  let serveraddr = head addrinfos
  socket' <- socket (addrFamily serveraddr)
                Stream defaultProtocol
  bind socket' (addrAddress serveraddr)
  listen socket' 1
  logAndEcho socket'
  close socket'




