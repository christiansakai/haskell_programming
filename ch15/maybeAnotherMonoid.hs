import Test.QuickCheck
import Data.Monoid

data Optional a =
    Nada
  | Only a
  deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  -- mempty :: Optional a
  mempty = Nada

  -- mappend :: Optional a -> Optional a -> Optional a
  mappend Nada _ = Nada
  mappend _ Nada = Nada
  mappend (Only x) (Only y) = Only (x `mappend` y)

instance Arbitrary a => Arbitrary (Optional a) where
  -- arbitrary :: Gen (Optional a)
  arbitrary = do
    a <- arbitrary
    frequency [ (1, return Nada)
              , (3, return (Only a))
              ]

newtype First' a =
  First' { getFirst' :: Optional a}
  deriving (Eq, Show)

-- | This monoid instance does not
-- require for the contents to be 
-- a monoid
instance Monoid (First' a) where
  -- mempty :: First' a
  mempty = First' Nada

  -- mappend :: First' a -> First' a -> First' a
  mappend x@(First' (Only _)) _ = x
  mappend (First' Nada) x       = x

instance Arbitrary a => Arbitrary (First' a) where
  -- arbitrary :: Gen (First' a)
  arbitrary = do
    a <- arbitrary
    return (First' a)

firstMappend :: First' a
             -> First' a
             -> First' a
firstMappend = mappend

type FirstMappend = First' String
                 -> First' String
                 -> First' String
                 -> Bool

type FstId = First' String -> Bool

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = 
  (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = 
  (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a =
  (a <> mempty) == a

main :: IO ()
main = do
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: FstId)
  quickCheck (monoidRightIdentity :: FstId)
