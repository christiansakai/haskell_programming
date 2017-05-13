newtype Constant a b =
  Constant { getConstant :: a }
  deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  -- fmap :: (b -> c) -> Constant a b -> Constant a c
  fmap f (Constant a) = Constant a 

-- instance 
--   Monoid a => 
--   Applicative (Constant a) where
--     -- pure :: x -> Constant a x
--     pure a = Constant a

--     -- (<*>) :: Constant a (b -> c) -> Constant d e -> Constant f g
--     (<*>) (Constant a) (Constant b) = Constant (a b)

