{-# LANGUAGE FlexibleInstances #-}

-- Rearrange the argument

-- 1
data Sum b a =
    First a
  | Second b

instance Functor (Sum a) where
  -- fmap :: (x -> y) -> Sum a x -> Sum a y
  fmap f (First a) = First (f a)
  fmap f (Second b) = Second b

-- 2
data Company a c b =
    DeepBlue a c
  | Something b

instance Functor (Company e e') where
  --fmap :: (x -> y) -> Company a b x -> Company a b y
  fmap f (Something b) = Something (f b)
  fmap _ (DeepBlue a c) = DeepBlue a c

-- 3
data More b a = 
    L a b a
  | R b a b
  deriving (Eq, Show)

instance Functor (More x) where
  -- fmap :: (x -> y) -> More a x -> More a y
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'

-- -- Write Functor instances
-- 1
data Quant a b =
    Finance
  | Desk a
  | Bloor b

instance Functor (Quant a) where
  -- fmap :: (x -> y) -> Quant a x -> Quant a y
  fmap f Finance = Finance
  fmap f (Desk a) = Desk a
  fmap f (Bloor b) = Bloor (f b)

-- 2
data K a b = K a

instance Functor (K a) where
  -- fmap :: (x -> y) = K a x -> K a y
  fmap f (K a) = K a

-- 3
newtype Flip f a b =
  Flip (f b a)
  deriving (Eq, Show)

newtype K' a b =
  K' a

instance Functor (Flip K' a) where
  -- fmap :: (x -> y) -> Flip K' a x -> Flip K' a y
  fmap f (Flip (K' a)) = Flip (K' (f a)) 

-- 4
data EvilGoateeConst a b =
  GoatyConst b

instance Functor (EvilGoateeConst a) where
  -- fmap :: (x -> y) -> EvilGoateeConst a x -> EvilGoateeConst a y
  fmap f (GoatyConst b) = GoatyConst (f b)

-- 5
data LiftItOut f a =
  LiftItOut (f a)

instance Functor f => Functor (LiftItOut f) where
  -- fmap :: (a -> b) -> LiftItOut f a -> LiftItOut f b
  fmap f (LiftItOut fa) = LiftItOut (fmap f fa)

-- 6
data Parappa f g a =
  DaWrappa (f a) (g a)

instance ( Functor f
         , Functor g
         ) => Functor (Parappa f g) where
  -- fmap :: (a -> b) -> Parappa f g a -> Parappa f g b
  fmap f (DaWrappa fa ga) = DaWrappa (fmap f fa) (fmap f ga)

-- 7
data IgnoreOne f g a b =
  IgnoringSomething (f a) (g b)

instance Functor g => Functor (IgnoreOne f g a) where
  -- fmap :: (x -> y) -> IgnoreOne f g a x -> IgnoreOne f g a y
  fmap f (IgnoringSomething fa ga) =
    IgnoringSomething fa (fmap f ga)

-- 8
data Notorious g o a t =
  Notorious (g o) (g a) (g t)

instance Functor g => Functor (Notorious g o a) where
  -- fmap :: (t -> p) -> Notorious g o a t -> Notorious g o a p
  fmap f (Notorious go ga gt) = Notorious go ga (fmap f gt)

-- 9
data List a = 
    Nil
  | Cons a (List a)

instance Functor List where
  -- fmap :: (a -> b) -> List a -> List b
  fmap f Nil = Nil
  fmap f (Cons a la) = Cons (f a) (fmap f la)

-- 10
data GoatLord a = 
    NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a)
              (GoatLord a)
              (GoatLord a)

instance Functor GoatLord where
  -- fmap :: (a -> b) -> GoatLord a -> GoatLord b
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat a) = OneGoat (f a)
  fmap f (MoreGoats gl gl' gl'') = 
    MoreGoats (fmap f gl)
              (fmap f gl')
              (fmap f gl'')
                                            
-- 11
data TalkToMe a = 
    Halt
  | Print String a
  | Read (String -> a)

instance Functor TalkToMe where
  -- fmap :: (a -> b) -> TalkToMe a -> TalkToMe b
  fmap _ Halt = Halt
  fmap f (Print s a) = Print s (f a)
  fmap f (Read g) = Read (f . g)
