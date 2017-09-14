newtype Constant a b =
  Constant { getConstant :: a }
  deriving (Eq, Ord, Show)

instance Functor (Constant e) where
  -- fmap :: (a -> b) -> Constant e a -> Constant e a
  fmap f (Constant x) = Constant x

instance Monoid e => Applicative (Constant e) where
  -- pure :: a -> Constant e a
  pure a = Constant mempty

  -- (<*>) :: Constant e (a -> b) -> Constant e a -> Constant e b
  -- (<*>) = undefined
  Constant e <*> Constant _ = Constant e
