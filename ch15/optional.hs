import Data.Monoid

data Optional a =
    Nada
  | Only a
  deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  -- mempty :: a
  mempty = Nada

  -- mappend :: a -> a -> a
  mappend x Nada            = x
  mappend Nada x            = x
  mappend (Only a) (Only b) = Only (mappend a b)
