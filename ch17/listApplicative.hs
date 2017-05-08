data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap f Nil        = Nil
  fmap f (Cons a l) = Cons (f a) (fmap f l)

instance Applicative List where
  pure = Cons
  -- (Cons a al) <*> (Cons b bl) = 
