import Data.List

lefts' :: [Either a b] -> [a]
lefts' = foldr onlyLeft []
  where onlyLeft (Left x) acc = x:acc
        onlyLeft _        acc = acc

rights' :: [Either a b] -> [b]
rights' = foldr onlyRight []
  where onlyRight (Right x) acc = x:acc
        onlyRight _         acc = acc

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers'     []       = ([], [])
partitionEithers' (Left x:xs)  = let (lefts, rights) = partitionEithers' xs
                                  in (x:lefts, rights)
partitionEithers' (Right x:xs) = let (lefts, rights) = partitionEithers' xs
                                  in (lefts, x:rights)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Left a)  = Nothing
eitherMaybe' f (Right b) = Just $ f b

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' aToc _bToc (Left a)  = aToc a
either' _aToc bToc (Right b) = bToc b

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' bToc rb@(Right b) = Just $ either' undefined bToc rb
eitherMaybe'' _    _            = Nothing
