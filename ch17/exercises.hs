import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- 1
data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  -- fmap :: (a -> b) -> Pair a -> Pair b
  fmap f (Pair a a') = Pair (f a) (f a')

instance Applicative Pair where
  -- pure :: a -> Pair a
  pure a = Pair a a 

  -- (<*>) :: Pair (a -> b) -> Pair a -> Pair b
  (<*>) (Pair f f') (Pair a a') = Pair (f a) (f' a')

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    return (Pair a a)

instance Eq a => EqProp (Pair a) where
  -- eq :: Eq a => Pair a -> Pair a -> Property
  -- (=-=) :: Pair a -> Pair a -> Property
  (=-=) = eq

checkPair :: IO ()
checkPair = do
  let trigger = undefined :: Pair (Bool, Int, String)
  quickBatch $ functor trigger
  quickBatch $ applicative trigger


-- 2
data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  -- fmap :: (b -> c) -> Two a b -> Two a c
  fmap f (Two a b) = Two a (f b)

instance Monoid a => Applicative (Two a) where
  -- pure :: b -> Two a b
  pure b = Two mempty b 

  -- (<*>) :: Two f (b -> c) -> Two a b -> Two a c
  Two f g <*> Two a b = Two a (g b)
  
instance ( Arbitrary a
         , Arbitrary b 
         ) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

instance Eq b => EqProp (Two a b) where
  -- (=-=) :: Two a b -> Two a b -> Property
  Two a b =-= Two a' b' = b `eq` b'
  
checkTwo :: IO ()
checkTwo = do
  let trigger = undefined :: Two String (Bool, Int, String)
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  

-- 3
data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  -- fmap :: (c -> d) -> Three a b c -> Three a b d
  fmap f (Three a b c) = Three a b (f c)

instance ( Monoid a 
         , Monoid b
         ) => Applicative (Three a b) where
  -- pure :: c -> Three a b c
  pure c = Three mempty mempty c

  -- (<*>) :: Three f g (c -> d) -> Three a b c -> Three a b d
  Three f g h <*> Three a b c = Three a b (h c)

instance ( Arbitrary a
         , Arbitrary b
         , Arbitrary c
         ) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three a b c)

instance Eq c => EqProp (Three a b c) where
  -- (=-=) :: Three a b c -> Three a b c -> Property
  Three a b c =-= Three a' b' c' = c `eq` c'

checkThree :: IO ()
checkThree = do
  let trigger = undefined :: Three String String (Bool, Int, String)
  quickBatch $ functor trigger
  quickBatch $ applicative trigger


-- 4
data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  -- fmap :: (b -> c) -> Three' a b -> Three' a c
  fmap f (Three' a b b') = Three' a (f b) (f b')

instance Monoid a => Applicative (Three' a) where
  -- pure :: b -> Three' a b
  pure b = Three' mempty b b

  -- (<*>) :: Three' f (b -> c) (b -> c) -> Three' a b b -> Three' a c c
  Three' f g g' <*> Three' a b b' = Three' a (g b) (g' b')

instance ( Arbitrary a
         , Arbitrary b
         ) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Three' a b b)

instance Eq b => EqProp (Three' a b) where
  -- (=-=) :: Three' a b -> Three' a b -> Property
  Three' a b b' =-= Three' x y y' = b `eq` y

checkThree' :: IO ()
checkThree' = do
  let trigger = undefined :: Three' String (Bool, Int, String)
  quickBatch $ functor trigger
  quickBatch $ applicative trigger


-- 5
data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  -- fmap :: (d -> e) -> Four a b c d -> Four a b c e
  fmap f (Four a b c d) = Four a b c (f d)

instance ( Monoid a
         , Monoid b 
         , Monoid c
         ) => Applicative (Four a b c) where
  -- pure :: d -> Four a b c d
  pure d = Four mempty mempty mempty d

  -- (<*>) :: Four f g h (d -> e) -> Four a b c d -> Four a b c e
  Four f g h i <*> Four a b c d = Four a b c (i d)

instance ( Arbitrary a
         , Arbitrary b
         , Arbitrary c
         , Arbitrary d
         ) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary 
    return $ Four a b c d

instance Eq d => EqProp (Four a b c d) where
  -- (=-=) :: Four a b c d -> Four a' b' c' d' -> Property
  Four a b c d =-= Four a' b' c' d' = d `eq` d'

checkFour :: IO ()
checkFour = do
  let trigger = undefined :: Four String String String (Bool, Int, String)
  quickBatch $ functor trigger
  quickBatch $ applicative trigger


-- 6
data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  -- fmap :: (b -> c) -> Four' a b -> Four' a c
  fmap f (Four' a a' a'' b) = Four' a a' a'' (f b)

instance Monoid a => Applicative (Four' a) where
  -- pure :: b -> Four' a b
  pure b = Four' mempty mempty mempty b

  -- (<*>) :: Four' f (b -> c) -> Four' a b -> Four' a c
  Four' f f' f'' g <*> Four' a a' a'' b = Four' a a' a'' (g b)

instance ( Arbitrary a
         , Arbitrary b
         ) => Arbitrary (Four' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Four' a a a b

instance Eq b => EqProp (Four' a b) where
  -- (=-=) :: Four' a a a b -> Four' a a a b -> Property
  Four' a a' a'' b =-= Four' c c' c'' d = b `eq` d

checkFour' :: IO ()
checkFour' = do
  let trigger = undefined :: Four' String (Bool, Int, String) where
  quickBatch $ functor trigger
  quickBatch $ applicative trigger



main = do
  checkPair
  checkTwo
  checkThree
  checkThree'
  checkFour
  checkFour'

