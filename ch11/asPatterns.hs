import Data.Char (toUpper)

isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf []        []      = True
isSubsequenceOf []        ys      = True
isSubsequenceOf _         []      = False
isSubSequenceOf ax@(x:xs) (y:ys)
  | x == y    = isSubsequenceOf xs ys
  | otherwise = isSubsequenceOf ax ys

capitalizeWords :: String -> [(String, String)]
capitalizeWords xs = map f $ words xs
  where f as@(s:st) = (as, toUpper s : st)
