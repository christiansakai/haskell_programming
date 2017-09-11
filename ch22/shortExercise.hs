import Data.Char

cap :: [Char] -> [Char]
cap = map toUpper

rev :: [Char] -> [Char]
rev = reverse

composed :: [Char] -> [Char]
composed = cap . rev

fmapped :: [Char] -> [Char]
fmapped = fmap cap rev

tupled :: [Char] -> ([Char], [Char])
tupled x = (cap x, rev x)

tupled' :: [Char] -> ([Char], [Char])
tupled' = ((,) <$> cap <*> rev)

tupled'' :: [Char] -> ([Char], [Char])
tupled'' =
  



