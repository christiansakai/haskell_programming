import Test.QuickCheck
import Test.QuickCheck.Function

functorIdentity :: ( Functor f
                   , Eq (f a) ) => f a 
                                -> Bool
functorIdentity f =
  fmap id f == f

functorCompose :: ( Eq (f c)
                  , Functor f ) => f a
                                -> Fun a b
                                -> Fun b c
                                -> Bool
functorCompose x (Fun _ f) (Fun _ g) =
  (fmap (g . f) x) == (fmap g . fmap f $ x)


-- 1
newtype Identity a = Identity a deriving (Eq, Show)

instance Arbitrary a => 
         Arbitrary (Identity a) where
  -- arbitrary :: Gen (Identity a)
  arbitrary = do
    a <- arbitrary
    return (Identity a)

instance Functor Identity where
  -- fmap :: (a -> b) -> Identity a -> Identity b
  fmap f (Identity a) = Identity (f a)

type IdentityIdentity = Identity Int -> Bool

type IdentityCompose = Identity Int
                    -> Fun Int Bool
                    -> Fun Bool String
                    -> Bool
              
checkIdentity :: IO ()
checkIdentity = do
  quickCheck (functorIdentity :: IdentityIdentity)
  quickCheck (functorCompose :: IdentityCompose)

-- 2
data Pair a = Pair a a deriving (Eq, Show)

instance (Arbitrary a) => Arbitrary (Pair a) where
  -- arbitrary :: Gen (Pair a)
  arbitrary = do
    a <- arbitrary
    return $ Pair a a

instance Functor Pair where
  -- fmap :: (a -> b) -> Pair a -> Pair b
  fmap f (Pair a a') = Pair (f a) (f a')

type PairIdentity = Pair Int -> Bool

type PairCompose = Pair Int 
                -> Fun Int Bool 
                -> Fun Bool String 
                -> Bool

checkPair :: IO ()
checkPair = do
  quickCheck (functorIdentity :: PairIdentity)
  quickCheck (functorCompose :: PairCompose)

-- 3
data Two a b = Two a b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  -- arbitrary :: Gen (Two a b)
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

instance Functor (Two a) where
  -- fmap :: (a -> b) -> Two a a' -> Two b b'
  fmap f (Two a a') = Two a (f a')

type TwoIdentity = Two String Int -> Bool

type TwoCompose = Two String Int
               -> Fun Int Bool
               -> Fun Bool Int
               -> Bool

checkTwo :: IO ()
checkTwo = do
  quickCheck (functorIdentity :: TwoIdentity)
  quickCheck (functorCompose :: TwoCompose)

-- 4
data Three a b c = Three a b c deriving (Eq, Show)

instance ( Arbitrary a
         , Arbitrary b
         , Arbitrary c
         ) => Arbitrary (Three a b c) where
  -- arbitrary :: Gen (Three a b c)
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

instance Functor (Three a b) where
  -- fmap :: (a -> b) -> Three x y a -> Three x y b
  fmap f (Three a b c) = Three a b (f c)

type ThreeIdentity = Three String Int String -> Bool

type ThreeCompose = Three String Int String
                 -> Fun String Bool
                 -> Fun Bool Int
                 -> Bool

checkThree :: IO ()
checkThree = do
  quickCheck (functorIdentity :: ThreeIdentity)
  quickCheck (functorCompose :: ThreeCompose)

-- 5
data Three' a b = Three' a b b deriving (Eq, Show)

instance ( Arbitrary a
         , Arbitrary b
         ) => Arbitrary (Three' a b) where
  -- arbitrary :: Gen (Three a b)
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    b' <- arbitrary
    return $ Three' a b b'

instance Functor (Three' a) where
  -- fmap :: (x -> y) -> Three' a x -> Three' a y
  fmap f (Three' a b b') = Three' a (f b) (f b')

type ThreeIdentity' = Three' String Int -> Bool

type ThreeCompose' = Three' String Int
                 -> Fun Int Bool
                 -> Fun Bool Int
                 -> Bool

checkThree' :: IO ()
checkThree' = do
  quickCheck (functorIdentity :: ThreeIdentity')
  quickCheck (functorCompose :: ThreeCompose')

-- 6
data Four a b c d = Four a b c d deriving (Eq, Show)

instance ( Arbitrary a
         , Arbitrary b
         , Arbitrary c
         , Arbitrary d
         ) => Arbitrary (Four a b c d) where
  -- arbitrary :: Gen (Four a b c d)
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four a b c d

instance Functor (Four a b c) where
  -- fmap :: (x -> y) -> Four a b c x -> Four a b c y
  fmap f (Four a b c x) = Four a b c (f x)

type FourIdentity = Four String Int String Bool -> Bool

type FourCompose = Four String Int String Bool
                 -> Fun Bool String
                 -> Fun String Int
                 -> Bool

checkFour :: IO ()
checkFour = do
  quickCheck (functorIdentity :: FourIdentity)
  quickCheck (functorCompose :: FourCompose)

-- 7
data Four' a b = Four' a a a b deriving (Eq, Show)

instance ( Arbitrary a
         , Arbitrary b
         ) => Arbitrary (Four' a b ) where
  -- arbitrary :: Gen (Four' a b )
  arbitrary = do
    a <- arbitrary
    a' <- arbitrary
    a'' <- arbitrary
    b <- arbitrary
    return $ Four' a a' a'' b

instance Functor (Four' a) where
  -- fmap :: (x -> y) -> Four x b -> Four y b
  fmap f (Four' a a' a'' b) = Four' a a' a'' (f b)

type FourIdentity' = Four' String Int -> Bool

type FourCompose' = Four' String Int
                 -> Fun Int String
                 -> Fun String Int
                 -> Bool

checkFour' :: IO ()
checkFour' = do
  quickCheck (functorIdentity :: FourIdentity')
  quickCheck (functorCompose :: FourCompose')

-- 8
-- Can't be implemented because
-- Trivial is not kind of (* -> *)

main :: IO ()
main = do
  checkIdentity
  checkPair
  checkTwo
  checkThree
  checkThree'
  checkFour
  checkFour'
