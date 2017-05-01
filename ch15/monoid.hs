import Data.Semigroup
import Test.QuickCheck

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c =
  (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = 
  (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a =
  (a <> mempty) == a

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  Trivial <> Trivial = Trivial

instance Monoid Trivial where
  mempty = Trivial
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
  -- quickCheck (monoidAssoc :: Trivial -> Bool)
  -- quickCheck (semigroupAssoc :: Trivial -> Bool)

-- data Optional a = Nada
--                 | Only a
--                 deriving (Eq, Show)

-- instance Monoid a => Monoid (Optional a) where
--   -- mempty :: Optional a
--   mempty = Nada

--   -- mappend :: a -> a -> a
--   mappend (Only a) Nada     = Only a
--   mappend Nada (Only a)     = Only a
--   mappend (Only a) (Only b) = Only (mappend a b)

-- newtype First' a = First' { getFirst' :: Optional a }
--                    deriving (Eq, Show)

-- instance Monoid a => Monoid (First' a) where
--   mempty = First' Nada

--   mappend (First' Nada) x = x
--   mappend x (First' Nada) = x
--   mappend (First' (Only a)) (First' (Only b)) = First' (Only a `mappend` a)


-- firstMappend :: First' a 
--              -> First' a
--              -> First' a
-- firstMappend = mappend

-- type FirstMappend = First' String 
--                  -> First' String
--                  -> First' String
--                  -> Bool

-- type FirstId =
--   First' String -> Bool

main :: IO ()
main = do
  checkTrivial
                
            


