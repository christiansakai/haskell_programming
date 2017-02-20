notThe :: String -> Maybe String
notThe "the" = Nothing
notThe xs    = Just xs

replaceThe :: String -> String
replaceThe = unwords . go . words
  where go []     = []
        go (x:xs) = case notThe x of
                      Nothing -> "a" : go xs
                      _       -> x : go xs

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = go . words
  where go []                                      = 0
        go [x]                                     = 0
        go listOfWords@(x:x':xs) 
          | x == "the" && (isVowel . head $ x') = 1 + go xs
          | otherwise                           = 0 + (go . tail $ listOfWords)
          where isVowel x = x `elem` ['a', 'i', 'o', 'u', 'e']

countVowels :: String -> Integer
countVowels ""      = 0
countVowels (x:xs)   
  | isVowel x = 1 + countVowels xs
  | otherwise = 0 + countVowels xs
  where isVowel x = x `elem` ['a', 'i', 'o', 'u', 'e']
