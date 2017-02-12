module Reverse where

rvrs :: String -> String
rvrs xs = let awesome = drop 9 xs
              is = drop 6 (take 8 xs)
              curry = take 5 xs
           in awesome ++ " " ++ is ++ " " ++ curry

main :: IO ()
main = print $ rvrs "Curry is awesome"
