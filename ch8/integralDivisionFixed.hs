type Numerator = Integer
type Denominator = Integer
type Quotient = Integer

data DividedResult = Result Quotient
                   | DividedByZero
                   deriving Show

dividedBy :: Numerator -> Denominator -> DividedResult
dividedBy num denum
        | denum == 0 = DividedByZero
        | otherwise = Result (div num denum)
