myZip :: [a] -> [b] -> [(a, b)]
myZip as     []     = []
myZip []     bs     = [] 
myZip (a:as) (b:bs) = (a, b) : myZip as bs

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f as     []     = []
myZipWith f []     bs     = []
myZipWith f (a:as) (b:bs) = f a b : myZipWith f as bs

myZip' :: [a] -> [b] -> [(a, b)]
myZip' = myZipWith (,)
