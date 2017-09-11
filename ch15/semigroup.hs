import Test.QuickCheck
import Data.Semigroup 
import Data.Monoid hiding ((<>))

semigroupAssoc :: (Eq m, Semigroup m)
               => m -> m -> m -> Bool
semigroupAssoc a b c =
  (a <> (b <> c)) == ((a <> b) <> c)

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
checkTrivial = 
  quickCheck (semigroupAssoc :: TrivialAssoc)


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

type IdentityAssoc a = Identity a -> Identity a -> Identity a -> Bool

checkIdentity :: IO ()
checkIdentity =
  quickCheck (semigroupAssoc :: IdentityAssoc String)


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

type TwoAssoc a b = Two a b -> Two a b -> Two a b -> Bool

checkTwo :: IO ()
checkTwo = 
  quickCheck (semigroupAssoc :: TwoAssoc String String)


-- 4
data Three a b c = Three a b c deriving (Eq, Show)

instance ( Arbitrary a
         , Arbitrary b
         , Arbitrary c ) => Arbitrary (Three a b c) where
  -- arbitrary :: Gen (Three a b c)
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

instance ( Semigroup a
         , Semigroup b
         , Semigroup c ) => Semigroup (Three a b c) where
  -- (<>) :: Three a b c -> Three a b c -> Three a b c
  (Three a b c) <> (Three a' b' c') = Three (a <> a') (b <> b') (c <> c')
 
type ThreeAssoc a b c = Three a b c -> Three a b c -> Three a b c -> Bool

checkThree :: IO ()
checkThree =
  quickCheck (semigroupAssoc :: ThreeAssoc String String String)


-- 5
data Four a b c d = Four a b c d deriving (Eq, Show)

instance ( Arbitrary a
         , Arbitrary b
         , Arbitrary c
         , Arbitrary d ) => Arbitrary (Four a b c d) where
  -- arbitrary :: Gen (Four a b c d)
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four a b c d

instance
  ( Semigroup a
  , Semigroup b
  , Semigroup c
  , Semigroup d ) => Semigroup (Four a b c d) where
  -- (<>) :: Four a b c d -> Four a b c d -> Four a b c d
  (Four a b c d) <> (Four a' b' c' d') =
    Four (a <> a') (b <> b') (c <> c') (d <> d')

type FourAssoc a b c d = Four a b c d -> Four a b c d -> Four a b c d -> Bool

checkFour :: IO ()
checkFour =
  quickCheck (semigroupAssoc :: FourAssoc String String String String)


-- 6
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

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

checkBoolConj :: IO ()
checkBoolConj = 
  quickCheck (semigroupAssoc :: BoolConjAssoc)


-- 7
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

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

checkBoolDisj :: IO ()
checkBoolDisj =
  quickCheck (semigroupAssoc :: BoolDisjAssoc)


-- 8
data Or a b =
    Fst a
  | Snd b
  deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  -- arbitrary :: Gen (Or a b)
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [ (1, return (Fst a))
              , (1, return (Snd b)) 
              ]

instance Semigroup (Or a b) where
  -- (<>) :: Or a b -> Or a b -> Or a b
  Fst a <> Snd b = Snd b
  Fst a <> Fst b = Fst b
  Snd a <> Fst b = Snd a
  Snd a <> Snd b = Snd b

type OrAssoc a b = Or a b -> Or a b -> Or a b -> Bool

checkOr :: IO ()
checkOr =
  quickCheck (semigroupAssoc :: OrAssoc String String)


-- 9
newtype Combine a b = 
  Combine { unCombine :: (a -> b) }

instance Semigroup b => Semigroup (Combine a b) where
  -- (<>) :: Combine a b -> Combine a b -> Combine a b
  (Combine f) <> (Combine g) =
    Combine (\x -> f x <> g x)
    
-- 10


-- 11
data Validation a b =
    MyFailure a
  | MySuccess b
  deriving (Eq, Show)

instance ( Arbitrary a
         , Arbitrary b ) => Arbitrary (Validation a b) where
  -- arbitrary :: Gen (Or a b)
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [ (1, return (MyFailure a))
              , (1, return (MySuccess b)) 
              ]

instance Semigroup (Validation a b) where
  -- (<>) :: Validation a b -> Validation a b -> Validation a b
  (MySuccess a) <> (MySuccess a') = MySuccess a'
  _ <> (MyFailure b) = MyFailure b
  (MyFailure a) <> _ = MyFailure a

type ValidationAssoc a b = Validation a b -> Validation a b -> Validation a b -> Bool

checkValidation :: IO ()
checkValidation = 
  quickCheck (semigroupAssoc :: ValidationAssoc String String)


-- 12
newtype AccumulateRight a b =
  AccumulateRight (Validation a b) deriving (Eq, Show)

instance ( Arbitrary a
         , Arbitrary b ) => Arbitrary (AccumulateRight a b) where
  -- arbitrary :: Gen (Or a b)
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [ (1, return (AccumulateRight (MyFailure a)))
              , (1, return (AccumulateRight (MySuccess b))) ]

instance Semigroup b => Semigroup (AccumulateRight a b) where
  -- (<>) :: AccumulateRight a b ->
  --         AccumulateRight a b ->
  --         AccumulateRight a b
  (AccumulateRight (MyFailure a)) <> _ = 
    AccumulateRight (MyFailure a)
  (AccumulateRight (MySuccess b)) <> (AccumulateRight (MyFailure a)) = 
    AccumulateRight (MyFailure a)
  (AccumulateRight (MySuccess b)) <> (AccumulateRight (MySuccess b')) =
    AccumulateRight (MySuccess (b <> b'))

type AccumulateRightAssoc a b = AccumulateRight a b -> AccumulateRight a b -> AccumulateRight a b -> Bool

checkAccumulateRight :: IO ()
checkAccumulateRight = 
  quickCheck (semigroupAssoc :: AccumulateRightAssoc String String)


-- 13
newtype AccumulateBoth a b =
  AccumulateBoth (Validation a b) deriving (Eq, Show)

instance ( Arbitrary a
         , Arbitrary b ) => Arbitrary (AccumulateBoth a b) where
  -- arbitrary :: Gen (Or a b)
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [ (1, return (AccumulateBoth (MyFailure a)))
              , (1, return (AccumulateBoth (MySuccess b))) ]

instance ( Semigroup a
         , Semigroup b ) => Semigroup (AccumulateBoth a b) where
  -- (<>) :: AccumulateBoth a b ->
  --         AccumulateBoth a b ->
  --         AccumulateBoth a b
  (AccumulateBoth (MyFailure a)) <> (AccumulateBoth (MyFailure a')) =
    AccumulateBoth (MyFailure (a <> a'))
  (AccumulateBoth (MyFailure a)) <> (AccumulateBoth (MySuccess b)) =
    AccumulateBoth (MyFailure a)
  (AccumulateBoth (MySuccess b)) <> (AccumulateBoth (MySuccess b')) =
    AccumulateBoth (MySuccess (b <> b'))
  (AccumulateBoth (MySuccess b)) <> (AccumulateBoth (MyFailure a)) =
    AccumulateBoth (MyFailure a)

type AccumulateBothAssoc a b = AccumulateBoth a b -> AccumulateBoth a b -> AccumulateBoth a b -> Bool

checkAccumulateBoth :: IO ()
checkAccumulateBoth = 
  quickCheck (semigroupAssoc :: AccumulateBothAssoc String String)


main :: IO ()
main = do
  checkTrivial
  checkIdentity
  checkTwo
  checkThree
  checkFour
  checkBoolConj
  checkBoolDisj
  checkOr
  checkValidation
  checkAccumulateRight
  checkAccumulateBoth
