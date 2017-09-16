newtype State s a =
  State { runState :: s -> (a, s) }

instance Functor (State s) where
  -- fmap :: (a -> b) -> State s a -> State s b
  fmap aTob (State sToas) = State sToab
    where sToab s = (aTob a, s)
            where a = fst . sToas $ s

instance Applicative (State s) where
  -- pure :: a -> State s a
  pure a = State $ \s -> (a, s)

  -- (<*>) :: State s (a -> b) -> State s a -> State s b
  State f <*> State g = State sb
    where sb s = let (aTob, _) = f s
                     (a, _) = g s
                 in (aTob a, s)

instance Monad (State s) where
  -- return :: a -> State s a
  return a = State $ \s -> (a, s)

  -- (>>=) :: State s a -> (a -> State s b) -> State s b
  State sa >>= f = let (a, s) = sa s
                    in f a

-- 1

get :: State s s
get = State $ \s -> (s, s)

-- 2

put :: s -> State s ()
put s = State $ \s -> ((), s)

-- 3

exec :: State s a -> s -> s
exec (State sa) = snd . sa
  
-- 4
eval :: State s a -> s -> a
eval (State sa) = fst . sa

-- 5
modify :: (s -> s) -> State s ()
modify f = State $ \s -> ((), f s)
