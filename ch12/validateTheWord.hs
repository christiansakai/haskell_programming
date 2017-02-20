newtype Word' = Word' String
  deriving (Eq, Show)

vowels = "aeiou"

countVowels :: String -> Int
countVowels ""      = 0
countVowels (x:xs)   
  | isVowel x = 1 + countVowels xs
  | otherwise = 0 + countVowels xs
  where isVowel x = x `elem` vowels

mkWord :: String -> Maybe Word'
mkWord xs = let vowels = countVowels xs
                consonants = length xs - vowels
             in case consonants > vowels of
                  True -> Just (Word' xs)
                  _    -> Nothing
