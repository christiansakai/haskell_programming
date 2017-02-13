module PoemLines where

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

splitBy :: Char -> String -> [String]
splitBy separator ""     = []
splitBy separator string = head : splitBy separator rest
  where head = takeWhile (/= separator) string
        rest = dropWhile (== separator) . dropWhile (/= separator) $ string

shouldEqual = ["Tyger Tyger, burning bright"
              , "In the forests of the night"
              , "What immortal hand or eye"
              , "Could frame thy fearful symmetry?"]

myLines :: String -> [String]
myLines = splitBy '\n'

main :: IO ()
main = 
  print $ "Are they equal ? " ++ show (myLines sentences == shouldEqual)
