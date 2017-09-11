newtype Constant a b =
  Constant { getConstant :: a }
  deriving (Eq, Ord, Show)

instance Functor (Constant e) where
  -- fmap :: (a -> b) -> Constant e a -> Constant e a
  fmap f (Constant x) = Constant x
