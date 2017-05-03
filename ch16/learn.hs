{-# LANGUAGE RankNTypes #-}

-- incMaybe :: Num a => Maybe a -> Maybe a
-- incMaybe m = fmap (+1) m

-- showMaybe :: Show a => Maybe a -> Maybe String
-- showMaybe s = fmap show s

-- liftedInc :: ( Functor f
--              , Num b ) => f b -> f b
-- liftedInc = fmap (+1)

-- liftedShow :: ( Functor f
--               , Show b ) => f b -> f String
-- liftedShow = fmap show 
      

-- incIfRight :: Num a => Either e a -> Either e a
-- incIfRight (Right n) = Right $ n + 1
-- incIfRight (Left e)  = Left e

-- showIfRight :: Show a => Either e a -> Either e String
-- showIfRight (Right s) = Right $ show s
-- showIfRight (Left e)  = Left e

-- incEither :: Num a => Either e a -> Either e a
-- incEither m = fmap (+1) m

-- showEither :: Show a => Either e a -> Either e String
-- showEither s = fmap show s

-- incMaybe :: Num a => Maybe a -> Maybe a
-- incMaybe m = fmap (+1) m

-- showMaybe :: Show a => Maybe a -> Maybe String
-- showMaybe s = fmap show s

-- liftedInc :: ( Functor f
--              , Num b ) => f b -> f b
-- liftedInc = fmap (+1)

-- liftedShow :: ( Functor f
--               , Show b ) => f b -> f String
-- liftedShow = fmap show 
      

-- incIfRight :: Num a => Either e a -> Either e a
-- incIfRight (Right n) = Right $ n + 1
-- incIfRight (Left e)  = Left e

-- showIfRight :: Show a => Either e a -> Either e String
-- showIfRight (Right s) = Right $ show s
-- showIfRight (Left e)  = Left e

-- incEither :: Num a => Either e a -> Either e a
-- incEither m = fmap (+1) m

-- showEither :: Show a => Either e a -> Either e String
-- showEither s = fmap show s



-- newtype Constant a b =
  -- Constant { getConstant :: a }
  -- deriving (Eq, Show)

-- instance Functor (Constant m) where
  -- fmap _ (Constant v) = Constant v



-- data Wrap f a =
--   Wrap (f a)
--   deriving (Eq, Show)

-- instance Functor (Wrap f) where
--   fmap f (Wrap fa) = Wrap (f fa)

-- instance Functor (Wrap f) where
--   fmap f (Wrap fa) = Wrap (fmap f fa)

-- instance Functor f => Functor (Wrap f) where
--   fmap f (Wrap fa) = Wrap (fmap f fa)
--



-- getInt :: IO Int
-- getInt = fmap read getLine


-- meTooIsm :: IO String
-- meTooIsm = do
--   input <- getLine
--   return (input ++ " and me too!")


-- bumpIt :: IO Int
-- bumpIt = do
--   intVal <- getInt
--   return (intVal + 1)


-- -- type Nat f g =
-- --   forall a. f a -> g a

type Nat f g =
  forall a . f a -> g a

maybeToList :: Nat Maybe []
maybeToList Nothing = []
maybeToList (Just a) = [a]

degenerateMtl :: Nat Maybe []
degenerateMtl Nothing = []
degenerateMtl (Just a) = [a + 1]

