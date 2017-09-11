-- 1
data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  -- fmap :: (x -> y) -> Sum a x -> Sum a y
  fmap f (First a) = First a
  fmap f (Second b) = Second (f b)

-- 2
-- Because the First and Either's Left is 
-- not a higher kinded type (* -> *)
