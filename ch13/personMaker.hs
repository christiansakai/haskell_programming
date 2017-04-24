type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid = NameEmpty
                   | AgeTooLow
                   | PersonInvalidUnknown String
                   deriving (Eq, Show)

mkPerson :: Name
         -> Age
         -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 = Right $ Person  name age
  | name == ""    = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise = Left $ PersonInvalidUnknown $
                       "Name was: " ++ show name ++
                       " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
  putStrLn "Give me name: "
  name <- getLine
  putStrLn "Give me age: "
  age <- getLine

  case mkPerson name (read age :: Integer) of
    Left personInvalid ->
      case personInvalid of
        NameEmpty ->
          putStrLn "Name is empty"

        AgeTooLow ->
          putStrLn "Age is too low"

        PersonInvalidUnknown reason ->
          putStrLn reason


    Right person ->
      putStrLn $ "Yay! Successfully got a person:" ++
                 show person
