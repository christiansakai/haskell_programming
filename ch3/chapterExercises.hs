addBang :: String -> String
addBang xs = xs ++ "!"

getFourthCharacter :: String -> Char
getFourthCharacter xs = xs !! 4

dropFirstNineCharacters :: String -> String
dropFirstNineCharacters xs = drop 9 xs

thirdLetter :: String -> Char
thirdLetter xs = xs !! 3

letterIndex :: Int -> Char
letterIndex x = "Curry is awesome" !! x

rvrs :: String -> String
rvrs xs = let awesome = drop 9 xs
              is = drop 6 (take 8 xs)
              curry = take 5 xs
           in awesome ++ " " ++ is ++ " " ++ curry
