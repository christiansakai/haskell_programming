tensDigit :: Integral a => a -> a
tensDigit x = d
  where (_, d) = x `divMod` 10

hunsD :: Integral a => a -> a
hunsD x = d2
  where (d2, _) = x `divMod` 10

foldBool :: a -> a -> Bool -> a
foldBool x y True   = x
foldBool x y False  = y

foldBool' :: a -> a -> Bool -> a
foldBool' x y bool = case bool of
                       True -> x
                       _    -> y

foldBool'' :: a -> a -> Bool -> a
foldBool x y bool   
  | bool = x
  | otherwise    = y

g :: (a -> b) -> (a, c) -> (b, c)
g aTob (x1, x2) (y1, y2) = (aTob x1, y2)
