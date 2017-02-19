import Data.Char (toUpper, isAlpha)
import Data.List (groupBy)
import Data.Function (on)

capitalizeWord :: String -> String
capitalizeWord ""       = ""
capitalizeWord full@(x:xs)   
  | isAlpha x = toUpper x : xs
  | otherwise = full

capitalizeParagraph :: String -> String
capitalizeParagraph ""        = ""
capitalizeParagraph paragraph = concat . map capitalize $ splitted
  where capitalize = snd . capitalize
        splitted   = filter (/=".") $ groupBy ((==) `on` (=='.')) paragraph
