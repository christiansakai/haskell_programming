newtype Moi s a =
  Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
  -- fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap aTob (Moi sToas) = Moi sToab
    where sToab s = (aTob a, s)
            where a = fst . sToas $ s

instance Applicative (Moi s) where
  -- pure :: a -> Moi s a
  pure a = Moi $ \s -> (a, s)

  -- (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
  Moi f <*> Moi g = Moi sb
    where sb = \s ->
                let (aTob, _) = f s
                    (a, _) = g s
                 in (aTob a, s)

instance Monad (Moi s) where
  -- return :: a -> Moi s a
  return a = Moi $ \s -> (a, s)

  -- (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
  Moi sa >>= f = let (a, s) = sa s
                  in f a
