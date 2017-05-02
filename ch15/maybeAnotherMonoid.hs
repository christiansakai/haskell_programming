import Test.QuickCheck
import Data.Monoid



monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c =
  (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = 
  (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a =
  (a <> mempty) == a



data Optional a =
    Nada
  | Only a
  deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  -- mempty :: a
  mempty = Nada

  -- mappend :: a -> a -> a
  mappend x Nada            = x
  mappend Nada x            = x
  mappend (Only a) (Only b) = Only (mappend a b)

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = do
    a <- arbitrary
    frequency [ (1, return Nada)
              , (3, return (Only a))]



newtype First' a =
  First' { getFirst' :: Optional a }
  deriving (Eq, Show)

instance Monoid (First' a) where
  -- mempty :: a
  mempty = First' Nada

  -- mappend :: a -> a -> a
  mappend (First' x) (First' y) = First' x

firstMappend :: First' a
             -> First' a
             -> First' a
firstMappend = mappend

type FirstMappend = 
    First' String
 -> First' String
 -> First' String
 -> Bool

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = do
    a <- arbitrary
    return (First' a)

type FstId = First' String -> Bool

main :: IO ()
main = do
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: FstId)
  quickCheck (monoidRightIdentity :: FstId)
