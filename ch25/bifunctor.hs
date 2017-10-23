{-# LANGUAGE KindSignatures #-}

class Bifunctor (p :: * -> * -> *) where
  {-# MINIMAL bimap | first, second #-}

  bimap :: (a -> b)
        -> (c -> d)
        -> p a c
        -> p b d
  bimap f g = first f . second g

  first :: (a -> b) -> p a c -> p b c
  first f = bimap f id

  second :: (c -> d) -> p a c -> p a d
  second = bimap id

data Deux a b = Deux a b

-- 1
instance Bifunctor Deux where
  -- bimap :: (a -> b) -> (c -> d) -> Deux a c -> Deux b d
  bimap f g (Deux a c) = Deux (f a) (g c)

  -- first :: (a -> b) -> Deux a c -> Deux b c
  first f (Deux a c) = Deux (f a) c

  -- second :: (c -> d) -> Deux a c -> Deux a d
  second f (Deux a c) = Deux a (f c)

-- 2
data Const a b = Const a

instance Bifunctor Const where
  -- bimap :: (a -> b) -> (c -> d) -> Const a c -> Const b c
  bimap f g (Const a) = Const (f a)

  -- first :: (a -> b) -> Const a c -> Const b c
  first f (Const a) = Const (f a)

  -- second :: (c -> d) -> Const a c -> Const a c
  second f (Const a) = Const a


-- 3
data Drei a b c = Drei a b c

instance Bifunctor (Drei e) where
  -- bimap :: (a -> b) -> (c -> d) -> Drei e a c -> Deux e b d
  bimap f g (Drei e a c) = Drei e (f a) (g c)

  -- first :: (a -> b) -> Drei e a c -> Drei e b c
  first f (Drei e a c) = Drei e (f a) c

  -- second :: (c -> d) -> Drei e a c -> Drei e a d
  second f (Drei e a c) = Drei e a (f c)

-- 4
data SuperDrei a b c = SuperDrei a b

instance Bifunctor (SuperDrei e) where
  -- bimap :: (a -> b) -> (c -> d) -> SuperDrei e a c -> SuperDrei e b d
  bimap f g (SuperDrei e a) = SuperDrei e (f a)

  -- first :: (a -> b) -> SuperDrei e a c -> SuperDrei e b c
  first f (SuperDrei e a) = SuperDrei e (f a)

  -- second :: (c -> d) -> SuperDrei e a c -> SuperDrei e a d
  second f (SuperDrei e a) = SuperDrei e a

-- 5
data SemiDrei a b c = SemiDrei a

instance Bifunctor (SemiDrei e) where
  -- bimap :: (a -> b) -> (c -> d) -> SemiDrei e a c -> SemiDrei e b d
  bimap f g (SemiDrei e) = SemiDrei e

  -- first :: (a -> b) -> SemiDrei e a c -> SemiDrei e b c
  first f (SemiDrei e) = SemiDrei e

  -- second :: (c -> d) -> SemiDrei e a c -> SemiDrei e a d
  second f (SemiDrei e) = SemiDrei e

-- 6
data Quadriceps a b c d = Quadzzz a b c d

instance Bifunctor (Quadriceps a b) where
  -- bimap :: (a -> b) -> (c -> d) -> Quadriceps e e' a c -> Quadriceps e e' b d
  bimap f g (Quadzzz e e' a c) = Quadzzz e e' (f a) (g c)

  -- first :: (a -> b) -> Quadriceps e e' a c -> Quadriceps e e' b c
  first f (Quadzzz e e' a c) = Quadzzz e e' (f a) c

  -- second :: (c -> d) -> Quadriceps e e' a c -> Quadriceps e e' a d
  second f (Quadzzz e e' a c) = Quadzzz e e' a (f c)

-- 7
data MyEither a b = 
    MyLeft a
  | MyRight b

instance Bifunctor MyEither where
  -- bimap :: (a -> b) -> (c -> d) -> Either a c -> Either b d
  bimap f g (MyLeft a) = MyLeft (f a)
  bimap f g (MyRight c) = MyRight (g c)

  -- first :: (a -> b) -> Either a c -> Either b c
  first f (MyLeft a) = MyLeft (f a)
  first f (MyRight c) = MyRight c

  -- second :: (c -> d) -> Either a c -> Either a d
  second f (MyLeft a) = MyLeft a
  second f (MyRight c) = MyRight (f c)



