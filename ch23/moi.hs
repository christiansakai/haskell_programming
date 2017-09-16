newtype Moi s a =
  Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
  -- fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi sa) = Moi sb
    where sb s = let (a, s) = sa s
                  in (f a, s)
            
instance Applicative (Moi s) where
  -- pure :: a -> Moi s a
  pure = undefined

  -- (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
  Moi sab <*> Moi sa = Moi sb
    where sb s = let (a, _) = sa s
                     (f, _) = sab s
                  in (f a, s)

instance Monad (Moi s) where
  -- (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
  Moi sa >>= f = Moi sb
    where sb s = let (a, _) = sa s
                  in runMoi (f a) s

  -- return :: a -> Moi s a
  return a = Moi sa
    where sa s = (a, s)
