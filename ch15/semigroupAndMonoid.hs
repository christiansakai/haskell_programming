import Test.QuickCheck
import Data.Semigroup 
import Data.Monoid hiding ((<>))

semigroupAssoc :: ( Eq m
                  , Semigroup m ) => m
                                  -> m
                                  -> m
                                  -> Bool
semigroupAssoc a b c =
  (a <> (b <> c)) == ((a <> b) <> c)

monoidAssoc :: ( Eq m
               , Semigroup m
               , Monoid m ) => m 
                            -> m 
                            -> m 
                            -> Bool
monoidAssoc a b c =
  (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: ( Eq m
                      , Semigroup m
                      , Monoid m ) => m 
                                   -> Bool
monoidLeftIdentity a = 
  (mempty <> a) == a

monoidRightIdentity :: ( Eq m
                       , Semigroup m
                       , Monoid m ) => m 
                                    -> Bool
monoidRightIdentity a =
  (a <> mempty) == a


-- 1
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  -- (<>) :: Trivial -> Trivial -> Trivial
  Trivial <> Trivial = Trivial

instance Monoid Trivial where 
  -- mempty :: Trivial
  mempty = Trivial

  -- mappend :: Trivial -> Trivial -> Trivial
  mappend = (<>) 

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivialAssoc = Trivial 
                 -> Trivial
                 -> Trivial
                 -> Bool

checkTrivial :: IO ()
checkTrivial = do
  quickCheck (semigroupAssoc :: TrivialAssoc)
  quickCheck (monoidLeftIdentity :: Trivial -> Bool) 
  quickCheck (monoidRightIdentity :: Trivial -> Bool)



-- 2
newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => 
         Semigroup (Identity a) where
  -- (<>) :: Identity a -> Identity a -> Identity a
  (Identity a) <> (Identity b) = Identity (a <> b)

instance (Semigroup a, Monoid a) => 
         Monoid (Identity a) where
  -- mempty :: Identity a
  mempty = Identity mempty -- mempty is actually ()
                           -- so this is Identity ()

  -- mappend :: Identity a -> Identity a -> Identity a
  mappend = (<>)

instance Arbitrary a => 
         Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)

type IdentityAssoc = Identity String
                  -> Identity String
                  -> Identity String
                  -> Bool

checkIdentity :: IO ()
checkIdentity = do
  quickCheck (semigroupAssoc :: IdentityAssoc)
  quickCheck (monoidLeftIdentity :: Identity String -> Bool) 
  quickCheck (monoidRightIdentity :: Identity String -> Bool)



-- 3
data Two a b = Two a b deriving (Eq, Show)

instance ( Semigroup a
         , Semigroup b ) => Semigroup (Two a b) where
     -- (<>) :: Two a b -> Two a b -> Two a b
    (Two a b) <> (Two x y) = Two (a <> x) (b <> y)

instance 
  ( Semigroup a
  , Semigroup b
  , Monoid a
  , Monoid b ) => Monoid (Two a b) where
  -- mempty :: Two a b
  mempty = Two mempty mempty

  -- mappend :: Two a b -> Two a b -> Two a b
  mappend = (<>)
         
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

checkTwo :: IO ()
checkTwo = do
  quickCheck (semigroupAssoc :: TwoAssoc)
  quickCheck (monoidLeftIdentity :: Two String String -> Bool) 
  quickCheck (monoidRightIdentity :: Two String String -> Bool)


-- 4
data Three a b c = Three a b c deriving (Eq, Show)

instance ( Semigroup a
         , Semigroup b
         , Semigroup c ) => Semigroup (Three a b c) where
   (Three a b c) <> (Three x y z) = 
     Three (a <> x) ( b <> y) (c <> z)

instance ( Semigroup a
         , Semigroup b
         , Semigroup c
         , Monoid a
         , Monoid b
         , Monoid c
         ) => Monoid (Three a b c) where
  -- mempty :: Three a b c
  mempty = Three mempty mempty mempty

  -- mappend
  mappend = (<>)

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

checkThree :: IO ()
checkThree = do
  quickCheck (semigroupAssoc :: ThreeAssoc)
  quickCheck (monoidLeftIdentity :: Three String String String -> Bool) 
  quickCheck (monoidRightIdentity :: Three String String String -> Bool)


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

instance ( Semigroup a, Semigroup b, Semigroup c, Semigroup d
         , Monoid a, Monoid b, Monoid c, Monoid d
         ) => Monoid (Four a b c d ) where
  -- mempty :: Three a b c
  mempty = Four mempty mempty mempty mempty

  -- mappend
  mappend = (<>)

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

checkFour :: IO ()
checkFour = do
  quickCheck (semigroupAssoc :: FourAssoc)
  quickCheck (monoidLeftIdentity :: Four String String String String -> Bool) 
  quickCheck (monoidRightIdentity :: Four String String String String -> Bool)


-- 6
newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  (BoolConj True) <> (BoolConj True) = BoolConj True
  _ <> _                             = BoolConj False

instance Monoid BoolConj where
  -- mempty :: BoolConj
  mempty = BoolConj True

  -- mappend
  mappend = (<>)

instance Arbitrary BoolConj where
  arbitrary = do
    frequency [ (1, return (BoolConj True))
              , (1, return (BoolConj False)) ]

type BoolConjAssoc = BoolConj
                  -> BoolConj
                  -> BoolConj
                  -> Bool

checkBoolConj :: IO ()
checkBoolConj = do
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (monoidLeftIdentity :: BoolConj -> Bool) 
  quickCheck (monoidRightIdentity :: BoolConj -> Bool)


-- 7
newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  (BoolDisj False) <> (BoolDisj False) = BoolDisj False
  _ <> _                               = BoolDisj True

instance Monoid BoolDisj where
  -- mempty :: BoolDisj
  mempty = BoolDisj False

  -- mappend
  mappend = (<>)

instance Arbitrary BoolDisj where
  arbitrary = do
    frequency [ (1, return (BoolDisj True))
              , (1, return (BoolDisj False)) ]

type BoolDisjAssoc = BoolDisj
                  -> BoolDisj
                  -> BoolDisj
                  -> Bool

checkBoolDisj :: IO ()
checkBoolDisj = do
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  quickCheck (monoidLeftIdentity :: BoolDisj -> Bool) 
  quickCheck (monoidRightIdentity :: BoolDisj -> Bool)


-- 8
data Or a b = Fst a
            | Snd b
            deriving (Eq, Show)

instance Semigroup (Or a b) where
  (Fst a) <> (Fst b) = Fst b
  _ <> (Snd b)       = Snd b
  (Snd a) <> _       = Snd a

instance Monoid a => Monoid (Or a b) where
  -- mempty :: Or a b
  mempty = Fst mempty

  -- mappend :: Or a b -> Or a b -> Or a b
  mappend = (<>)

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

checkOr :: IO ()
checkOr = do
  quickCheck (semigroupAssoc :: OrAssoc)
  quickCheck (monoidLeftIdentity :: Or String String -> Bool) 
  -- NOT DONE YET
  -- verboseCheck (monoidRightIdentity :: Or String String -> Bool)

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
    (MySuccess a) <> (MySuccess b) = MySuccess a

instance ( Semigroup a, Semigroup b
         , Monoid a, Monoid b ) => Monoid (Validation a b) where
  -- mempty :: Validation a b
  mempty = MyFailure mempty

  -- mappend :: Validation a b -> Validation a b -> Validation a b
  mappend = (<>)

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

checkValidation :: IO ()
checkValidation = do
  quickCheck (semigroupAssoc :: ValidationAssoc)
  -- NOT DONE YET
  -- quickCheck (monoidLeftIdentity :: Validation String String -> Bool) 
  -- quickCheck (monoidRightIdentity :: Validation String String -> Bool)


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

instance ( Semigroup a, Semigroup b
         , Monoid a, Monoid b ) => Monoid (AccumulateRight a b) where
  -- mempty :: AccumulateRight a b
  mempty = AccumulateRight mempty

  -- mappend :: AccumulateRight a b -> AccumulateRight a b -> AccumulateRight a b
  mappend = (<>)

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

checkAccumulateRight :: IO ()
checkAccumulateRight = do
  quickCheck (semigroupAssoc :: AccumulateRightAssoc)
  -- quickCheck (monoidLeftIdentity :: AccumulateRight String String -> Bool) 
  -- quickCheck (monoidRightIdentity :: AccumulateRight String String -> Bool)

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

instance ( Semigroup a, Semigroup b
         , Monoid a, Monoid b 
         ) => Monoid (AccumulateBoth a b) where
  -- mempty :: AccumulateBoth a b
  mempty = AccumulateBoth mempty

  -- mappend :: AccumulateBoth a b -> AccumulateBoth a b -> AccumulateBoth a b
  mappend = (<>)


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

checkAccumulateBoth :: IO ()
checkAccumulateBoth = do
  quickCheck (semigroupAssoc :: AccumulateBothAssoc)
  quickCheck (monoidLeftIdentity :: AccumulateBoth String String -> Bool) 
  quickCheck (monoidRightIdentity :: AccumulateBoth String String -> Bool)

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
