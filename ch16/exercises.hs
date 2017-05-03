{-# LANGUAGE FlexibleInstances #-}

-- Rearrange the argument

-- 1
data Sum b a =
    First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum e) where
  fmap f (First a) = First (f a)
  fmap f (Second b) = Second b

-- 2
data Company a c b =
    DeepBlue a c
  | Something b

instance Functor (Company e e') where
  fmap f (Something b) = Something (f b)
  fmap _ (DeepBlue a c) = DeepBlue a c

-- 3
data More b a =
    L a b a
  | R b a b
  deriving (Eq, Show)

instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'


-- Write Functor instances

-- 1
data Quant a b =
    Finance
  | Desk a
  | Bloor b

instance Functor (Quant e) where
  fmap _ Finance = Finance
  fmap _ (Desk a) = Desk a
  fmap f (Bloor b) = Bloor (f b)

-- 2
data K a b =
  K a

instance Functor (K a) where
  fmap _ (K a) = K a 

-- 3
-- newtype Flip f a b =
--   Flip (f b a)
--   deriving (Eq, Show)

-- newtype K a b =
--   K a

-- instance Functor (Flip K a) where
  -- fmap f (Flip K a b) = Flip K (f a) b


-- 4
data EvilGoateeConst a b =
  GoatyConst b

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst (f b)

-- 5
-- data LiftItOut f a =
--   LiftItOut (f a)

-- instance Functor (LiftItOut f) where


-- 9
data List a = 
    Nil
  | Cons a (List a)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a as) = Cons (f a) (fmap f as)

-- 10
data GoatLord a =
    NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)

instance Functor GoatLord where
  fmap _ NoGoat             = NoGoat
  fmap f (OneGoat a)        = OneGoat (f a)
  fmap f (MoreGoats x y z)  = MoreGoats (fmap f x)
                                        (fmap f y)
                                        (fmap f z)

  

