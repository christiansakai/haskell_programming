import Data.Char (ord, chr, isLetter)

caesar :: String -> Int -> String
caesar ""     n  = ""
caesar (x:xs) n 
  | isLetter x = encode x n : caesar xs n
  | otherwise  = x : caesar xs n

encode :: Char -> Int -> Char
encode c n = chr $ (ord c + n - ord 'a' ) `mod` 26  + ord 'a'

decode :: Char -> Int -> Char
decode c n = chr $ (ord c - n - ord 'a') `mod` 26 + ord 'a'

uncaesar :: String -> Int -> String
uncaesar "" n     = ""
uncaesar (x:xs) n 
  | isLetter x = decode x n : uncaesar xs n
  | otherwise  = x : uncaesar xs n

main :: IO ()
main = do
  putStrLn $ "Enter a sentence to be encoded:" 
  word <- getLine
  putStrLn $ "Enter a number as the shifter: "
  number <- getLine
  putStrLn $ caesar word (read number :: Int)
