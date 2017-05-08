-- newtype Constant a b =
--   Constant { getConstant :: a }
--   deriving (Eq, Ord, Show)

-- instance Functor (Constant a) where
--   fmap f (Constant a b) = Constant a (f b)

-- instance Monoid a => 
--          Applicative (Constant a) where
--            pure = Constant a
--            (Constant a f) (<*>) (Constant a' b) = Constant a (f b)

