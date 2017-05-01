module ListyInstances where

import Data.Monoid
import Listy

instance Monoid (Listy a) where
  -- mempty :: a
  mempty = Listy []

  -- mappend :: a -> a -> a
  mappend (Listy l) (Listy l') = Listy $ mappend l l'
