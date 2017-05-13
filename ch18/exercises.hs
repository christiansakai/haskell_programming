import Prelude hiding (Either, Left, Right)
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- 1
data Nope a = NopeDotJpg 
  deriving (Eq, Show)

instance Functor Nope where
  -- fmap :: (a -> b) -> Nope a -> Nope b
  fmap _ NopeDotJpg = NopeDotJpg

instance Applicative Nope where
  -- pure :: a -> Nope a
  pure _ = NopeDotJpg

  -- (<*>) :: Nope (a -> b) -> Nope a -> Nope b
  NopeDotJpg <*> NopeDotJpg = NopeDotJpg

instance Monad Nope where
  -- (>>=) :: Nope a -> (a -> Nope b) -> Nope b
  NopeDotJpg >>= f = NopeDotJpg

instance Arbitrary a => Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg 

instance Eq a => EqProp (Nope a) where
  -- (=-=) :: Nope a -> Nope a -> Property
  (=-=) = eq

checkNope :: IO ()
checkNope = do
  let trigger = undefined :: Nope (String, Bool, Int)
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger


-- 2
data PhhhbbtttEither b a =
    Left a
  | Right b
  deriving (Eq, Show)

instance Functor (PhhhbbtttEither e) where
  -- fmap :: (a -> b) -> PhhhbbtttEither e a -> PhhhbbtttEither e b
  fmap f (Right b) = Right b
  fmap f (Left a)  = Left (f a)

instance Applicative (PhhhbbtttEither e) where
  -- pure :: a -> PhhbbtttEither e a
  pure = Left

  -- (<*>) :: PhhbbtttEither e (a -> b) -> PhhbbtttEither e a -> PhhbbtttEither e b
  Right e <*> _ = Right e
  _ <*> Right e = Right e
  Left f <*> Left a = Left (f a)

instance Monad (PhhhbbtttEither e) where
  -- (>>=) :: PhhhbbtttEither e a -> (a -> PhhhbbtttEither e b) -> PhhhbbtttEither e b
  Right e >>= _ = Right e
  Left a >>= f  = f a

instance ( Arbitrary a
         , Arbitrary b 
         ) => Arbitrary (PhhhbbtttEither b a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [ (1, return (Left a))
              , (1, return (Right b))
              ]

instance ( Eq a
         , Eq b 
         ) => EqProp (PhhhbbtttEither b a) where
  (=-=) = eq

checkPhhhbbtttEither :: IO ()
checkPhhhbbtttEither = do
  let trigger = undefined :: PhhhbbtttEither (Bool, Int, String) (Bool, Int, String)
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger


-- 3
newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  -- fmap :: (a -> b) -> Identity a -> Identity b
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  -- pure :: a -> Identity a
  pure = Identity

  -- (<*>) :: Identity (a -> b) -> Identity a -> Identity b
  Identity f <*> Identity a = Identity (f a)

instance Monad Identity where
  -- (>>=) :: Identity a -> (a -> Identity b) -> Identity b
  Identity a >>= f = f a

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)

instance Eq a => EqProp (Identity a) where
  -- (=-=) :: Nope a -> Nope a -> Property
  (=-=) = eq

checkIdentity :: IO ()
checkIdentity = do
  let trigger = undefined :: Identity (Bool, Int, String)
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger


-- 4
data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Monoid (List a) where
  -- mempty :: List a
  mempty = Nil

  -- mappend :: List a -> List a -> List a
  mappend Nil a = a
  mappend a Nil = a
  mappend (Cons a la) lb = 
    Cons a (mappend la lb)

instance Functor List where
  -- fmap :: (a -> b) -> List a -> List b
  fmap _ Nil         = Nil
  fmap f (Cons a la) = Cons (f a) (fmap f la)

instance Applicative List where
  -- pure :: a -> List a
  pure a = Cons a Nil

  -- (<*>) :: List (a -> b) -> List a -> List b
  Nil <*> _        = Nil
  _ <*> Nil        = Nil
  Cons f lf <*> la = mappend (fmap f la) (lf <*> la)

instance Monad List where
  -- (>>=) :: List a -> (a -> List b) -> List b
  Nil >>= _       = Nil
  Cons a la >>= f = mappend (f a) (la >>= f)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [ (1, return Nil)
              , (1, return (Cons a b))
              ]

instance Eq a => EqProp (List a) where
  -- (=-=) :: List a -> List a -> Property
  (=-=) = eq

checkList :: IO ()
checkList = do
  let trigger = undefined :: List (Bool, Int, String)
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger


main :: IO ()
main = do
  checkNope
  checkPhhhbbtttEither
  checkIdentity
  checkList
