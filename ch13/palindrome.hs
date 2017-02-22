import Control.Monad
import Data.Char (toLower, isLetter)

makeLowerCase :: String -> String
makeLowerCase = map toLower

filterAlphabet :: String -> String
filterAlphabet = filter isLetter

palindrome :: IO ()
palindrome = forever $ do
  line <- getLine
  let cleaned = filterAlphabet . makeLowerCase $ line
  case (cleaned == reverse cleaned) of
    True -> putStrLn "It's a palindrome!"
    False -> putStrLn "Nope!"
