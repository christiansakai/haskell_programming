module WordNumber
    ( digitToWord
    , digits
    , wordNumber
    ) where


import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"

digits :: Int -> [Int]
digits n = reverse . go $ n
  where go n
          | n < 10    = [n]
          | otherwise = modulo : go division 
            where modulo = n `mod` 10
                  division = n `div` 10

wordNumber :: Int -> String
wordNumber n = concat . intersperse "-" . map digitToWord . digits $ n
