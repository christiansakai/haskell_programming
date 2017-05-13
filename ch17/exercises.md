-- Chapter Exercises
-- 1
pure :: a -> [a]
pure a = [a]

(<*>) :: [(a -> b)] -> [a] -> [b]
(<*>) fs as = [f a | f <- fs, a <- as]

-- 2
pure :: a -> IO a
pure = IO

(<*>) :: IO (a -> b) -> IO a -> IO b
(<*>) (IO f) (IO a) = IO (f b)

-- 3
pure :: b -> (,) a b
pure b = (,) mempty b

(<*>) :: (,) f (b -> c) -> (,) a b -> (,) a c
(f, g) <*> (a, b) = (a, g b)

-- 4
pure :: b -> (->) a b
pure b = (-) mempty b

(<*>) :: (->) f g -> (->) a b -> (->) a c
(->) f g <*> (->) a b = (->) a (g b)
