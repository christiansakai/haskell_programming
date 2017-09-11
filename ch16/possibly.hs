data Possibly a =
    LolNope
  | Yeppers a
  deriving (Eq, Show)

instance Functor Possibly where
  -- fmap :: (a -> b) -> Possibly a -> Possibly a
  fmap f LolNope = LolNope
  fmap f (Yeppers a) = Yeppers (f a)
