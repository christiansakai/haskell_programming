import Control.Monad ( forever )
import System.Exit ( exitSuccess )
import Data.Char ( toLower )

palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  let lowered = fmap toLower line1
  let filtered = [x | x <- lowered, x `elem` ['a'..'z']]
  case (filtered == reverse filtered) of
    True -> putStrLn "It's a palindrome!"
    False -> do
      putStrLn "Nope!"
      exitSuccess
