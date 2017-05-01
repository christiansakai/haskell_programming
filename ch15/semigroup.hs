import Test.QuickCheck
import Data.Semigroup

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

-- 1
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  Trivial <> Trivial = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

-- 2
newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (Identity a) <> (Identity b) = Identity (a <> b)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)

type IdentityAssoc = Identity String -> Identity String -> Identity String -> Bool


-- 3
data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two a b) <> (Two c d) = Two (a <> c) (b <> d)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

type TwoAssoc = Two String String -> Two String String -> Two String String -> Bool


-- 4
data Three a b c = Three a b c deriving (Eq, Show)

instance ( Semigroup a
         , Semigroup b
         , Semigroup c ) => Semigroup (Three a b c) where
  (Three a b c) <> (Three x y z) = Three (a <> x) (b <> y) (c <> z)

instance ( Arbitrary a
         , Arbitrary b 
         , Arbitrary c ) => Arbitrary (Three a b c ) where
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

instance ( Semigroup a
         , Semigroup b
         , Semigroup c
         , Semigroup d ) => Semigroup (Four a b c d) where
           (Four  a b c d) <> (Four  p q r t) = Four (a <> p) (b <> q) (c <> r) (d <> t)

instance ( Arbitrary a
         , Arbitrary b 
         , Arbitrary c
         , Arbitrary d) => Arbitrary (Four a b c d) where
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
newtype BoolConj =
  BoolConj Bool
  deriving (Eq, Show)

instance Semigroup BoolConj where
  (BoolConj True) <> (BoolConj True) = BoolConj True
  _ <> _ = BoolConj False

instance Arbitrary BoolConj where
  arbitrary = do
    bool <- arbitrary
    return (BoolConj bool)

type BoolConjAssoc = BoolConj
                  -> BoolConj
                  -> BoolConj
                  -> Bool


-- 7
newtype BoolDisj =
  BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  (BoolDisj False) <> (BoolDisj False) = BoolDisj False
  _ <> _ = BoolDisj True

instance Arbitrary BoolDisj where
  arbitrary = do
    bool <- arbitrary
    return (BoolDisj bool)

type BoolDisjAssoc = BoolDisj
                  -> BoolDisj
                  -> BoolDisj
                  -> Bool


-- 8
-- data Or a b = Fst a
--             | Snd b

-- instance Semigroup (Or a b) where
--   (Fst a) <> (Snd b) = Snd b
--   (Fst a) <> (Fst b) = Fst b
--   (Snd a) <> (Fst b) = Snd a
--   (Snd a) <> (Snd b) = Snd a

-- instance ( Arbitrary a
--          , Arbitrary b ) => Arbitrary (Or a b) where
--   arbitrary = do
--     a <- arbitrary
--     b <- arbitrary
--     return (Or a b)


main :: IO ()
main = do
  quickCheck (semigroupAssoc :: TrivialAssoc)
  quickCheck (semigroupAssoc :: IdentityAssoc)
  quickCheck (semigroupAssoc :: TwoAssoc)
  quickCheck (semigroupAssoc :: ThreeAssoc)
  quickCheck (semigroupAssoc :: FourAssoc)
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
