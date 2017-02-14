myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\x acc -> if f x then True else acc) False
  
myElem :: (Eq a) => a -> [a] -> Bool
myElem x = any (== x)

myElem' :: (Eq a) => a -> [a] -> Bool
myElem' x = foldr (\x' acc -> if x == x' then True else acc) False

myReverse :: [a] -> [a]
myReverse = foldr (\x acc -> acc ++ [x]) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\x acc -> f x : acc) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\x acc -> if f x then x : acc else acc) []

squish :: [[a]] -> [a]
squish = foldr (\x acc -> x ++ acc) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr (\x acc -> f x ++ acc) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [x]    = x
myMaximumBy f (x:xs) = foldr fo x xs
  where fo a b
          | f a b == GT = a
          | otherwise   = b

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ [x]       = x
myMinimumBy f (x:x':xs) = foldr fo x xs
  where fo a b
         | f a b == LT = a
         | otherwise   = b
