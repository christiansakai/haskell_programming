import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Control.Monad.IO.Class

main :: IO ()
main = do
  password <- runMaybeT getPassword
  case password of
    Just pass -> putStrLn "Valid password"
    _         -> putStrLn "Invalid password"

isValid :: String -> Bool
isValid = (>= 10) . length

getPassword :: MaybeT IO String
getPassword =
  liftIO getLine >>= MaybeT . checkPass
    where
      checkPass :: String -> IO (Maybe String)
      checkPass pass =
        case isValid pass of
          True -> return $ Just pass
          _    -> return Nothing

      _ = getLine >>= checkPass :: IO (Maybe String)

data A
