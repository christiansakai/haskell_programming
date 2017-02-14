fibs :: [Integer]
fibs = take 20 $ 1 : scanl (+) 1 fibs

first20Fibs :: [Integer]
first20Fibs = take 20 $ 1 : scanl (+) 1 first20Fibs

-- Still buggy
lessThan100Fibs :: [Integer]
lessThan100Fibs = filter (< 100) $ 1 : scanl (+) 1 lessThan100Fibs

fibsN :: Int -> Integer
fibsN = (!!) fibs

factorial :: (Num a, Enum a) => Int -> a
factorial x = (take (x + 1) $ scanl (*) 1 [1..]) !! x



