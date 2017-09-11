import Data.Monoid

data Optional a =
    Nada
  | Only a
  deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  -- mempty :: Optional a
  mempty = Nada

  -- mappend :: Optional a -> Optional a -> Optional a
  mappend Nada _  = Nada
  mappend _ Nada  = Nada
  mappend (Only a) (Only b) = Only (a `mappend` b)
