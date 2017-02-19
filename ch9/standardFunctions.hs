myAnd' :: [Bool] -> Bool
myAnd' []     = True
myAnd' (x:xs) = if x == False then False else myAnd xs

myAnd :: [Bool] -> Bool
myAnd []      = True
myAnd (x:xs)  = x && myAnd xs

myOr' :: [Bool] -> Bool
myOr' []      = False
myOr' (x:xs)  = if x == True then True else myOr' xs 

myOr :: [Bool] -> Bool
myOr []       = False
myOr (x:xs)   = x || myOr xs

myAny' :: (a -> Bool) -> [a] -> Bool
myAny' f []     = False
myAny' f (x:xs) = if f x then True else myAny' f xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny f []      = False
myAny f (x:xs)  = f x || myAny f xs

myElem' :: (Eq a) => a -> [a] -> Bool
myElem' x' = myAny (== x')

myElem :: (Eq a) => a -> [a] -> Bool
myElem _  []     = False
myElem x' (x:xs) = x' == x || myElem x' xs

myReverse :: [a] -> [a]
myReverse []      = []
myReverse (x:xs)  = myReverse xs ++ [x]

squish :: [[a]] -> [a]
squish []     = []
squish (x:xs) = x ++ squish xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f []      = []
squishMap f (x:xs)  = f x ++ squishMap f xs

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _          [x]       = x
myMaximumBy comparator (x:x':xs) = case comparator x x' of
                                     GT -> x
                                     _  -> myMaximumBy comparator (x':xs)

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _          [x]       = x
myMinimumBy comparator (x:x':xs) = case comparator x x' of
                                     LT -> x
                                     _  -> myMinimumBy comparator (x':xs)

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare
