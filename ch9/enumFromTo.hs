eftBool :: Bool -> Bool -> [Bool]
eftBool x y 
  | x == y    = [x]
  | otherwise = x : eftBool (succ x) y

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd x y
  | x == y    = [x]
  | otherwise = x : eftOrd (succ x) y

eftInt :: Int -> Int -> [Int]
eftInt x y
  | x == y    = [x]
  | otherwise = x : eftInt (succ x) y

eftChar :: Char -> Char -> [Char]
eftChar x y
  | x == y    = [x]
  | otherwise = x : eftChar (succ x) y
