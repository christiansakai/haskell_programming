import Test.QuickCheck
import Data.Semigroup 
import Data.Monoid hiding ((<>))

semigroupAssoc :: (Eq m, Semigroup m)
               => m -> m -> m -> Bool
semigroupAssoc a b c =
  (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Semigroup m, Monoid m) 
                   => m -> Bool
monoidLeftIdentity a = 
  (mempty <> a) == a

monoidRightIdentity :: (Eq m, Semigroup m, Monoid m) 
                    => m -> Bool
monoidRightIdentity a =
  (a <> mempty) == a

-- 1
data Trivial = Trivial deriving (Eq, Show)

instance Arbitrary Trivial where
  -- arbitrary :: Gen Trivial
  arbitrary = return Trivial

instance Semigroup Trivial where
  -- (<>) :: Trivial -> Trivial -> Trivial
  _ <> _ = Trivial

instance Monoid Trivial where
  -- mempty :: Trivial
  mempty = Trivial

  -- mappend :: Trivial -> Trivial -> Trivial
  mappend = (<>)

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

checkTrivial :: IO ()
checkTrivial = do
  quickCheck (semigroupAssoc :: TrivialAssoc)
  quickCheck (monoidLeftIdentity :: Trivial -> Bool) 
  quickCheck (monoidRightIdentity :: Trivial -> Bool)


-- 2 
newtype Identity a = Identity a deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Identity a) where
  -- arbitrary :: Gen (Identity a)
  arbitrary = do
    a <- arbitrary
    return (Identity a)

instance Semigroup a => Semigroup (Identity a) where
  -- (<>) :: Identity a -> Identity a -> Identity a
  (Identity x) <> (Identity y) = Identity (x <> y) 

instance (Semigroup a, Monoid a) => Monoid (Identity a) where
  -- mempty :: Identity a
  mempty = Identity mempty

  -- mappend :: Identity a -> Identity a -> Identity a
  mappend = (<>)

type IdentityAssoc a = Identity a -> Identity a -> Identity a -> Bool

checkIdentity :: IO ()
checkIdentity = do
  quickCheck (semigroupAssoc :: IdentityAssoc String)
  quickCheck (monoidLeftIdentity :: Identity String -> Bool) 
  quickCheck (monoidRightIdentity :: Identity String -> Bool)


-- 3
data Two a b = Two a b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  -- arbitrary :: Gen (Two a b)
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  -- (<>) :: Two a b -> Two a b -> Two a b
  (Two x y) <> (Two x' y') = Two (x <> x') (y <> y')

instance ( Semigroup a 
         , Semigroup b
         , Monoid a
         , Monoid b ) => Monoid (Two a b) where
  -- mempty :: Two a b
  mempty = Two mempty mempty

  -- mappend :: Two a b -> Two a b -> Two a b
  mappend = (<>)

type TwoAssoc a b = Two a b -> Two a b -> Two a b -> Bool

checkTwo :: IO ()
checkTwo = do
  quickCheck (semigroupAssoc :: TwoAssoc String String)
  quickCheck (monoidLeftIdentity :: Two String String -> Bool) 
  quickCheck (monoidRightIdentity :: Two String String -> Bool)


-- 4
newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Arbitrary BoolConj where
  -- arbitrary :: Gen BoolConj
  arbitrary = do
    a <- arbitrary
    return $ BoolConj a

instance Semigroup BoolConj where
  -- (<>) :: BoolConj -> BoolConj -> BoolConj
  (BoolConj True) <> (BoolConj True) = BoolConj True
  _ <> _ = BoolConj False

instance Monoid BoolConj where
  -- mempty :: BoolConj
  mempty = BoolConj True

  -- mappend :: BoolConj -> BoolConj -> BoolConj
  mappend = (<>)

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

checkBoolConj :: IO ()
checkBoolConj = do
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (monoidLeftIdentity :: BoolConj -> Bool) 
  quickCheck (monoidRightIdentity :: BoolConj -> Bool)


-- 5
newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Arbitrary BoolDisj where
  -- arbitrary :: Gen BoolDisj
  arbitrary = do
    a <- arbitrary
    return $ BoolDisj a

instance Semigroup BoolDisj where
  -- (<>) :: BoolDisj -> BoolDisj -> BoolDisj
  (BoolDisj False) <> (BoolDisj False) = BoolDisj False
  _ <> _ = BoolDisj True

instance Monoid BoolDisj where
  -- mempty :: BoolDisj
  mempty = BoolDisj False

  -- mappend :: BoolDisj -> BoolDisj -> BoolDisj
  mappend = (<>)

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

checkBoolDisj :: IO ()
checkBoolDisj = do
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  quickCheck (monoidLeftIdentity :: BoolDisj -> Bool) 
  quickCheck (monoidRightIdentity :: BoolDisj -> Bool)


-- 6
newtype Combine a b = Combine { unCombine :: (a -> b) }

-- 7
newtype Comp a = Comp { unComp :: (a -> a) }

-- 8
newtype Mem s a =
  Mem {
    runMem :: s -> (a,s)
  }

main :: IO ()
main = do
  checkTrivial
  checkIdentity
  checkTwo
  checkBoolConj
  checkBoolDisj
