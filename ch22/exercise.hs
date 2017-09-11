newtype Reader r a =
  Reader { runReader :: r -> a }

instance Monad (Reader r) where
  -- return :: a -> Reader r a
  return a = Reader $ const a

  -- (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  Reader ra >>= aRb = Reader $ aRb . ra
