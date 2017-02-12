mySum :: (Eq a, Num a) => a -> a
mySum n = go n 1 0
  where go n from total
           | n == from = total
           | otherwise = go n (from + 1) (total + n)

myMultiply :: (Integral a) => a -> a -> a
myMultiply x y = go x 0 0
  where go x from total
         | from == y = total
         | otherwise = go x (from + 1) (total + x)
