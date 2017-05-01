import Data.Monoid

data Optional a = Nada
                | Only a
                deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  -- mempty :: Optional a
  mempty = Nada

  -- mappend :: a -> a -> a
  mappend (Only a) Nada     = Only a
  mappend Nada (Only a)     = Only a
  mappend (Only a) (Only b) = Only (mappend a b)

  
