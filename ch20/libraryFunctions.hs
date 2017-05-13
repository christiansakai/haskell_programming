import Prelude hiding 
  ( sum
  , product
  , elem
  , minimum
  , maximum
  , null
  , length
  , toList
  )
import Data.Monoid
import Data.Traversable

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
elem a as = foldr compare False as
  where compare el acc
              | a == el = True
              | otherwise = False

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' a ta =
  getAny $ foldMap compare ta
    where compare el = 
            if a == el
               then Any True
               else Any False

-- 4
minimum :: (Foldable t, Ord a) => t a -> Maybe a
minimum as = 
  foldr checkVal Nothing as
    where checkVal el Nothing = Just el
          checkVal el (Just el')
                | el < el'  = Just el
                | otherwise = Just el'

-- 5
maximum :: (Foldable t, Ord a) => t a -> Maybe a
maximum as =
  foldr checkVal Nothing as
    where checkVal el Nothing = Just el
          checkVal el (Just el')
                  | el > el'  = Just el
                  | otherwise = Just el'

-- 6
null :: (Foldable t) => t a -> Bool
null ta = 
  foldr checkNull True ta
    where checkNull el _ = False

-- 7
length :: (Foldable t) => t a -> Int
length ta =
  foldr increment 0 ta
    where increment el acc = acc + 1

length' :: (Foldable t) => t a -> Int
length' ta =
  getSum $ foldMap add ta 
    where add el = Sum 1

-- 8
toList :: (Foldable t) => t a -> [a]
toList ta =
  foldr append [] ta
    where append el acc = el:acc

toList' :: (Foldable t) => t a -> [a]
toList' ta = 
  foldMap append ta
    where append el = [el]

-- 9
-- fold' :: (Foldable t, Monoid m) => t m -> m
-- fold' tm =  
  -- foldMap (\a -> 
