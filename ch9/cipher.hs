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

testSentence = "haskell is awesome"
testCaesared = caesar testSentence 3
testUncaesared = uncaesar testCaesared 3

main = 
  if testSentence == testUncaesared 
     then putStrLn "Cipher ok!"
     else putStrLn "Cipher NOT ok!"
