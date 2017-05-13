data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

append :: List a -> List a -> List a
append Nil la         = la
append (Cons a la) lb = Cons a (la `append` lb)

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil          = b
fold f b (Cons a la)  = f a (fold f b la)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f la = concat' $ fmap f la

instance Functor List where
  -- fmap :: Functor f => (a -> b) -> List a -> List b
  fmap f Nil          = Nil
  fmap f (Cons a la)  = Cons (f a) (fmap f la)

instance Applicative List where
  -- pure :: a -> List a
  pure a = Cons a Nil

  -- (<*>) :: List (a -> b) -> List a -> List b
  Nil <*> la          = Nil
  _ <*> Nil           = Nil
  (Cons f lf) <*> la  = 
    fmap f la `append` (lf <*> la)

