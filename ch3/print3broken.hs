module Print3Broken where

greeting :: String
greeting = "Yarrr"

printSecond :: IO ()
printSecond = do
  putStrLn greeting

main ::IO ()
main = do
  putStrLn greeting
  printSecond
