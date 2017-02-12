data Rocks = Rocks String 
  deriving (Eq, Show)

data Yeah = Yeah Bool
  deriving (Eq, Show)

data Papu = Papu Rocks Yeah
  deriving (Eq, Show)

-- Won't compile, no instance of function
-- phew = Papu "chases" True

truth = Papu (Rocks "chomskydoz")
             (Yeah True)

equalityForall :: Papu -> Papu -> Bool
equalityForall p p' = p == p'

-- Won't compile, no instance of Ord
-- comparePapus :: Papu -> Papu -> Bool
-- comparePapus p p' = p > p'


