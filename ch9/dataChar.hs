import Data.Char

filterAllUpperCase :: String -> String
filterAllUpperCase = filter (\x -> elem x ['A'..'Z'])

capitalizeFirstLetter :: String -> String
capitalizeFirstLetter []     = []
capitalizeFirstLetter (x:xs) = toUpper x : xs

capitalizeLetter :: String -> String
capitalizeLetter []     = []
capitalizeLetter (x:xs) = toUpper x : capitalizeLetter xs

getFirstLetterCapitalized :: String -> String
getFirstLetterCapitalized []    = [] 
getFirstLetterCapitalized (x:_) = [toUpper x]
