import Prelude hiding 
  ( sum
  , product
  , elem
  , null
  , length
  , toList
  , foldMap
  )
import Data.Monoid
import Data.Foldable hiding 
  ( minimum
  , maximum
  , null
  , length
  , toList
  , fold
  , foldMap
  )

-- 1
sum :: (Foldable t, Num a) => t a -> a
sum = foldr (+) 0

sum' :: (Foldable t, Num a) => t a -> a
sum' ta = getSum $ foldMap Sum ta

-- 2
product :: (Foldable t, Num a) => t a -> a
product = foldr (*) 1

product' :: (Foldable t, Num a) => t a -> a
product' ta = getProduct $ foldMap Product ta

-- 3
elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem a ta = foldr compare False ta
  where compare el acc
              | a == el = True
              | otherwise = False

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' a ta = getAny $ foldMap compare ta
  where compare el 
              | a == el = Any True
              | otherwise = Any False

-- 4
minimum :: (Foldable t, Ord a) => t a -> Maybe a
minimum ta = 
  foldr checkVal Nothing ta
    where checkVal el Nothing = Just el
          checkVal el (Just acc) =
            if el <= acc 
               then Just el
               else Just acc

-- 5
maximum :: (Foldable t, Ord a) => t a -> Maybe a
maximum ta = 
  foldr checkVal Nothing ta
    where checkVal el Nothing = Just el
          checkVal el (Just acc) =
            if el >= acc 
               then Just el
               else Just acc

-- 6
null :: (Foldable t) => t a -> Bool
null ta = foldr checkNull True ta
  where checkNull el acc = False

null' :: (Foldable t) => t a -> Bool
null' ta = getAny $ foldMap makeAny ta
  where makeAny el = Any True

-- 7
length :: (Foldable t) => t a -> Int
length ta = foldr increment 0 ta
  where increment el acc = acc + 1

length' :: (Foldable t) => t a -> Int
length' ta = getSum $ foldMap increment ta
  where increment el = Sum 1

-- 8
toList :: (Foldable t) => t a -> [a]
toList ta = foldr (:) [] ta

toList' :: (Foldable t) => t a -> [a]
toList' ta = foldMap makeList ta
  where makeList el = [el]

-- 9
fold :: (Foldable t, Monoid m) => t m -> m
fold tm = foldMap id tm

-- 10
foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap aTom ta = foldr doFold mempty ta
  where doFold el acc = aTom el <> acc

