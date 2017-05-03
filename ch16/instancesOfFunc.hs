import Test.QuickCheck
import Test.QuickCheck.Function


functorIdentity :: ( Functor f
                   , Eq (f a) ) => f a 
                                -> Bool
functorIdentity f =
  fmap id f == f


functorCompose :: ( Eq (f c)
                  , Functor f ) => (a -> b)
                                -> (b -> c)
                                -> f a
                                -> Bool
functorCompose f g x =
  (fmap g (fmap f x)) == (fmap (g . f) x)


functorCompose' :: ( Eq (f c)
                   , Functor f ) => f a
                                 -> Fun a b
                                 -> Fun b c
                                 -> Bool
functorCompose' x (Fun _ f) (Fun _ g) =
  (fmap (g . f) x) == (fmap g . fmap f $ x)


-- 1
newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Arbitrary a => 
         Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)

type IdentityId = Identity Int
               -> Bool

type IdentityCompose = Identity Int
                    -> Fun Int Int
                    -> Fun Int Int
                    -> Bool
              
checkIdentity :: IO ()
checkIdentity = do
  quickCheck (functorIdentity :: IdentityId)
  quickCheck (functorCompose' :: IdentityCompose)



-- 2
data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)

instance Arbitrary a =>
         Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    return (Pair a a)

type PairId = Pair String
           -> Bool

type PairCompose = Pair String
                -> Fun String String
                -> Fun String String
                -> Bool

checkPair :: IO ()
checkPair = do
  quickCheck (functorIdentity :: PairId)
  quickCheck (functorCompose' :: PairCompose)



-- 3
data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance 
  ( Arbitrary a
  , Arbitrary b
  ) => Arbitrary (Two a b) where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      return (Two a b)

type TwoId = Two Int String
          -> Bool

type TwoCompose = Two String Int
               -> Fun Int String
               -> Fun String String
               -> Bool

checkTwo :: IO ()
checkTwo = do
  quickCheck (functorIdentity :: TwoId)
  quickCheck (functorCompose' :: TwoCompose)



-- 4
data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance 
  ( Arbitrary a
  , Arbitrary b
  , Arbitrary c
  ) => Arbitrary (Three a b c) where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      c <- arbitrary
      return (Three a b c)

type ThreeId = Three Bool Int String 
            -> Bool

type ThreeCompose = Three Int String Bool
                 -> Fun Bool String
                 -> Fun String Int
                 -> Bool
          
checkThree :: IO ()
checkThree = do
  quickCheck (functorIdentity :: ThreeId)
  quickCheck (functorCompose' :: ThreeCompose)



-- 6
data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

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

type FourId = Four Bool Int String Char
           -> Bool

type FourCompose = Four Int Bool String Char
                -> Fun Char String
                -> Fun String Int
                -> Bool

checkFour :: IO ()
checkFour = do
  quickCheck (functorIdentity :: FourId)
  quickCheck (functorCompose' :: FourCompose)



main :: IO ()
main = do
  checkIdentity
  checkPair
  checkTwo
  checkThree
  checkFour
