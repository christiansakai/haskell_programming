type Numerator = Integer
type Denominator = Integer
type Quotient = Integer

dividedBy :: Numerator -> Denominator -> Quotient
dividedBy num denom = go num denum 0
  where go n d count
         | n < d = (count, n)
         | otherwise = go (n - d) d (count + 1)
