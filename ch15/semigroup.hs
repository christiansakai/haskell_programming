import Test.QuickCheck
import Data.Semigroup

semigroupAssoc :: ( Eq m
                  , Semigroup m ) => m
                                  -> m
                                  -> m
                                  -> Bool
semigroupAssoc a b c =
  (a <> (b <> c)) == ((a <> b) <> c)



-- 1
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  Trivial <> Trivial = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivialAssoc = Trivial 
                 -> Trivial
                 -> Trivial
                 -> Bool



-- 2
newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => 
         Semigroup (Identity a) where
  (Identity a) <> (Identity b) = Identity (a <> b)

instance Arbitrary a => 
         Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)

type IdentityAssoc = Identity String
                  -> Identity String
                  -> Identity String
                  -> Bool



-- 3
data Two a b = Two a b deriving (Eq, Show)

instance ( Semigroup a
         , Semigroup b ) => Semigroup (Two a b) where
    (Two a b) <> (Two x y) = Two (a <> x) (b <> y)

instance ( Arbitrary a
         , Arbitrary b ) => Arbitrary (Two a b) where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      return (Two a b)

type TwoAssoc = Two String String
             -> Two String String
             -> Two String String
             -> Bool


-- 4
data Three a b c = Three a b c deriving (Eq, Show)

instance ( Semigroup a
         , Semigroup b
         , Semigroup c ) => Semigroup (Three a b c) where
   (Three a b c) <> (Three x y z) = 
     Three (a <> x) ( b <> y) (c <> z)

instance ( Arbitrary a
         , Arbitrary b
  , Arbitrary c ) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three a b c) 

type ThreeAssoc = Three String String String
               -> Three String String String
               -> Three String String String
               -> Bool



-- 5
data Four a b c d = Four a b c d deriving (Eq, Show)

instance 
  ( Semigroup a
  , Semigroup b
  , Semigroup c
  , Semigroup d ) => Semigroup (Four a b c d) where
    (Four a b c d) <> (Four p q l m) = Four (a <> p)
                                            (b <> q)
                                            (c <> l)
                                            (d <> m)

instance
  ( Arbitrary a
  , Arbitrary b
  , Arbitrary c
  , Arbitrary d
  ) => Arbitrary (Four a b c d) where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      c <- arbitrary
      d <- arbitrary
      return (Four a b c d)

type FourAssoc = Four String String String String
              -> Four String String String String
              -> Four String String String String
              -> Bool


-- 6
newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  (BoolConj True) <> (BoolConj True) = BoolConj True
  _ <> _                             = BoolConj False

instance Arbitrary BoolConj where
  arbitrary = do
    frequency [ (1, return (BoolConj True))
              , (1, return (BoolConj False)) ]

type BoolConjAssoc = BoolConj
                  -> BoolConj
                  -> BoolConj
                  -> Bool



-- 7
newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  (BoolDisj False) <> (BoolDisj False) = BoolDisj False
  _ <> _                               = BoolDisj True

instance Arbitrary BoolDisj where
  arbitrary = do
    frequency [ (1, return (BoolDisj True))
              , (1, return (BoolDisj False)) ]

type BoolDisjAssoc = BoolDisj
                  -> BoolDisj
                  -> BoolDisj
                  -> Bool



-- 8
data Or a b = Fst a
            | Snd b
            deriving (Eq, Show)

instance Semigroup (Or a b) where
  (Fst a) <> (Fst b) = Fst b
  _ <> (Snd b)       = Snd b
  (Snd a) <> _       = Snd a

instance
  ( Arbitrary a
  , Arbitrary b
  ) => Arbitrary (Or a b) where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      frequency [ (1, return (Fst a))
                , (1, return (Snd b)) ]

type OrAssoc = Or Int Int
            -> Or Int Int
            -> Or Int Int
            -> Bool


-- 9
-- 10


-- 11
data Validation a b =
    MyFailure a 
  | MySuccess b
  deriving (Eq, Show)

instance
  ( Semigroup a
  , Semigroup b 
  ) => Semigroup (Validation a b) where
    (MyFailure a) <> _ = MyFailure a
    (MySuccess a) <> (MyFailure b) = MyFailure b
    (MySuccess a) <> (MySuccess b) = MySuccess (a <> b)

instance 
  ( Arbitrary a
  , Arbitrary b
  ) => Arbitrary (Validation a b) where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      frequency [ (1, return (MySuccess a))
                , (2, return (MyFailure b)) ]

type ValidationAssoc = Validation String String
                    -> Validation String String
                    -> Validation String String
                    -> Bool



-- 12
newtype AccumulateRight a b =
  AccumulateRight (Validation a b) deriving (Eq, Show)

instance Semigroup b => 
         Semigroup (AccumulateRight a b) where 
           (<>) (AccumulateRight (MyFailure a))
                (AccumulateRight (MySuccess b)) = 
                  AccumulateRight (MySuccess b)
           (<>) (AccumulateRight (MyFailure a))
                (AccumulateRight (MyFailure b)) =
                  AccumulateRight (MyFailure a)
           (<>) (AccumulateRight (MySuccess a))
                (AccumulateRight (MyFailure b)) =
                 AccumulateRight (MySuccess a)
           (<>) (AccumulateRight (MySuccess a))
                (AccumulateRight (MySuccess b)) = 
                  AccumulateRight (MySuccess (a <> b))

instance 
  ( Arbitrary a
  , Arbitrary b
  ) => Arbitrary (AccumulateRight a b) where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      frequency [ (1, return (AccumulateRight (MyFailure a)))
                , (1, return (AccumulateRight (MySuccess b))) ]

type AccumulateRightAssoc = AccumulateRight String String
                         -> AccumulateRight String String
                         -> AccumulateRight String String
                         -> Bool



-- 13
newtype AccumulateBoth a b =
  AccumulateBoth (Validation a b) deriving (Eq, Show)

instance
  ( Semigroup a
  , Semigroup b
  ) => Semigroup (AccumulateBoth a b) where
    (<>) (AccumulateBoth (MyFailure a))
         (AccumulateBoth (MyFailure b)) =
           AccumulateBoth (MyFailure (a <> b))
    (<>) (AccumulateBoth (MySuccess a))
         (AccumulateBoth (MySuccess b)) =
           AccumulateBoth (MySuccess (a <> b))
    (<>) (AccumulateBoth (MyFailure a))
         (AccumulateBoth (MySuccess b)) =
           AccumulateBoth (MySuccess b)
    (<>) (AccumulateBoth (MySuccess a))
         (AccumulateBoth (MyFailure b)) =
           AccumulateBoth (MySuccess a)

instance
  ( Arbitrary a
  , Arbitrary b
  ) => Arbitrary (AccumulateBoth a b) where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      frequency [ (1, return (AccumulateBoth (MyFailure a)))
                , (1, return (AccumulateBoth (MySuccess b))) ]

type AccumulateBothAssoc = AccumulateBoth String String
                        -> AccumulateBoth String String
                        -> AccumulateBoth String String
                        -> Bool


main :: IO ()
main = do
  quickCheck (semigroupAssoc :: TrivialAssoc)
  quickCheck (semigroupAssoc :: IdentityAssoc)
  quickCheck (semigroupAssoc :: TwoAssoc)
  quickCheck (semigroupAssoc :: ThreeAssoc)
  quickCheck (semigroupAssoc :: FourAssoc)
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  quickCheck (semigroupAssoc :: OrAssoc)
  quickCheck (semigroupAssoc :: ValidationAssoc)
  quickCheck (semigroupAssoc :: AccumulateRightAssoc)
  quickCheck (semigroupAssoc :: AccumulateBothAssoc)

