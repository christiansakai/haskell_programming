{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.Text
import qualified Data.Text.IO as T
import Data.Map as Map
import Control.Applicative

data LoginError = InvalidEmail
                | NoSuchUser
                | WrongPassword
                deriving Show

getDomain :: Text -> Either LoginError Text
getDomain email =
  case splitOn "@" email of
    [name, domain] -> Right domain
    _              -> Left InvalidEmail

printResult' :: Either LoginError Text -> IO ()
printResult' domain =
  case domain of
    Right text -> 
      T.putStrLn (append "Domain: " text)
    Left InvalidEmail -> 
      T.putStrLn "ERROR: Invalid domain"

printResult :: Either LoginError Text -> IO ()
printResult domain =
  T.putStrLn $ either (const "ERROR: Invalid domain")
                      (append "Domain: ") domain
  
getToken :: IO (Either LoginError Text)
getToken =
  T.putStrLn "Enter email address:" >>
    T.getLine >>= \line ->
      return (getDomain line)
     
users :: Map Text Text
users = Map.fromList
  [ ("example.com", "qwerty123")
  , ("localhost", "password")
  ]

userLogin :: IO (Either LoginError Text)
userLogin =
  getToken >>= \token ->
    case token of
      Right domain ->
        case Map.lookup domain users of
          Just userpw ->
            T.putStrLn "Enter password:" >>
              T.getLine >>= \password ->
                if userpw == password then 
                  return token
                else
                  return (Left WrongPassword)

          Nothing ->
            return (Left NoSuchUser)

      left ->
        return left

data ExceptIO e a = 
  ExceptIO { runExceptIO :: IO (Either e a) }

instance Functor (ExceptIO e) where
  fmap :: forall e a b . (a -> b) -> ExceptIO e a -> ExceptIO e b
  fmap f eio = 
    let unwrapped :: IO (Either e a)
        unwrapped = runExceptIO eio

        fmapped :: IO (Either e b)
        fmapped = fmap (fmap f) unwrapped

        wrapped :: ExceptIO e b
        wrapped = ExceptIO fmapped
        
     in wrapped
    
instance Applicative (ExceptIO e) where
  pure :: forall e a . a -> ExceptIO e a
  pure a = ExceptIO unwrap
    where
      unwrap :: IO (Either e a)
      unwrap = pure $ Right a

  (<*>) :: forall e a b . ExceptIO e (a -> b) -> ExceptIO e a -> ExceptIO e b
  eiof <*> eioa = 
    ExceptIO eiob'

      where
        eiof' :: IO (Either e (a -> b))
        eiof' = runExceptIO eiof

        eioa' :: IO (Either e a)
        eioa' = runExceptIO eioa

        -- eiob' :: IO (Either e b)
        -- eiob' = undefined

        -- fmap and <*> on IO
        -- 1
        _ = fmap :: (a -> b) -> IO a -> IO b

        -- 2
        _ = (<*>) :: IO (Either e a  -> Either e b) 
                  -> IO (Either e a) -> IO (Either e b)
  
        -- fmap and <*> on Either
        -- 3
        _ = fmap :: (Either e a -> Either e b) 
                 -> IO (Either e a) -> IO (Either e b)
     
        -- 4
        _ = (<*>) :: Either e (a -> b) -> Either e a -> Either e b

        -- fmap for outer IO and <*> for inner Either
        -- 5
        _ = fmap (<*>) :: IO (Either e (a -> b)) 
                       -> IO (Either e a -> Either e b)

        -- 6
        _ = fmap (<*>) eiof' :: IO (Either e a -> Either e b)

        -- We have 2, and we can use 6 as input to 2
        -- 7
        _ = (<*>) (fmap (<*>) eiof') :: IO (Either e a) -> IO (Either e b)

        -- we have IO (Either e a) which is eioa'
        -- 8
        _ = (<*>) (fmap (<*>) eiof') eioa' :: IO (Either e b)

        -- refactor no 8
        _ = (fmap (<*>) eiof') <*> eioa' :: IO (Either e b)

        -- no 8 is our eiob'
        eiob' = (fmap (<*>) eiof') <*> eioa' :: IO (Either e b)

instance Monad (ExceptIO e) where
  return :: a -> ExceptIO e a
  return = pure

  (>>=) :: forall e a b . ExceptIO e a -> (a -> ExceptIO e b) -> ExceptIO e b
  eioa >>= f = ExceptIO eiob'
    where
      eioa' :: IO (Either e a)
      eioa' = runExceptIO eioa

      eiob' :: IO (Either e b)
      eiob' = eioa' >>= func

      _ = f ::  (a -> ExceptIO e b) 

      -- fmap, <*>, return, >>= for IO
      -- 1
      _ = fmap :: (a -> b) -> IO a -> IO b

      -- 2
      _ = (<*>) :: IO (a -> b) -> IO a -> IO b

      -- 3
      _ = return :: a -> IO a

      -- 4
      _ = (>>=) :: IO a -> (a -> IO b) -> IO b

      -- fmap, <*>, return, >>= for Either
      -- 5
      _ = fmap :: (a -> b) -> Either e a -> Either e b

      -- 6
      _ = (<*>) :: Either e (a -> b) -> Either e a -> Either e b

      -- 7
      _ = return :: a -> Either e a

      -- 8
      _ = (>>=) :: Either e a -> (a -> Either e b) -> Either e b

      -- we know that we have IO (Either e a)
      -- maybe we can use bind for IO
      -- replace a with (Either e a)
      -- 9
      _ = (>>=) :: IO (Either e a) -> ((Either e a) -> IO b) -> IO b

      -- seems that we can replace IO b to become
      -- IO (Either e b)
      -- 10
      _ = (>>=) :: IO (Either e a) -> ((Either e a) -> IO (Either e b)) -> IO (Either e b)

      -- we want to have IO (Either e b) as end
      -- we have IO (Either e a) as input
      -- leave the function for type hole
      _ = (>>=) eioa' func :: IO (Either e b)

      -- time to see what func is
      func :: Either e a -> IO (Either e b)
      func either =
        case either of
          Left e -> 
            return (Left e)
              where 
                _ = Left e :: Either e b
                _ = return :: a -> IO a
                _ = return :: b -> IO b
                _ = return :: Either e b -> IO (Either e b)
                _ = return (Left e) :: IO (Either e b)

          Right a -> 
            runExceptIO (f a)

              where
                _ = f :: (a -> ExceptIO e b) 
                _ = f a :: ExceptIO e b
                _ = runExceptIO :: ExceptIO e a -> IO (Either e a)
                _ = runExceptIO :: ExceptIO e b -> IO (Either e b)
                _ = runExceptIO (f a) :: IO (Either e b)

getToken' :: ExceptIO LoginError Text
getToken' = do
  let _ = T.putStrLn "Enter email address: " :: IO ()
      _ = T.getLine :: IO Text
      _ = getDomain :: Text -> Either LoginError Text
      _ = getDomain (undefined :: Text) :: Either LoginError Text

      _ = fmap Right (T.putStrLn "Enter email address: ") :: IO (Either LoginError ())
      _ = fmap Right T.getLine :: IO (Either LoginError Text)
      _ = return (getDomain (undefined :: Text)) :: IO (Either LoginError Text)

      _ = ExceptIO $ fmap Right (T.putStrLn "Enter email address: ") :: ExceptIO LoginError ()
      _ = ExceptIO $ fmap Right T.getLine :: ExceptIO LoginError Text
      _ = ExceptIO $ return (getDomain (undefined :: Text)) :: ExceptIO LoginError Text

  ExceptIO $ fmap Right (T.putStrLn "Enter email address: ")
  input <- ExceptIO $ fmap Right T.getLine
  ExceptIO $ return (getDomain input)


liftEither :: Either e a -> ExceptIO e a
liftEither x = ExceptIO (pure x)

liftIO :: IO a -> ExceptIO e a
liftIO a = ExceptIO $ fmap Right a

getToken'' :: ExceptIO LoginError Text
getToken'' = do
  let _ = T.putStrLn "Enter email address: " :: IO ()
      _ = T.getLine :: IO Text
      _ = getDomain :: Text -> Either LoginError Text
      _ = getDomain (undefined :: Text) :: Either LoginError Text

  liftIO $ T.putStrLn "Enter email address: "
  input <- liftIO T.getLine
  liftEither (getDomain input)

userLogin'' :: ExceptIO LoginError Text
userLogin'' = do
  token <- getToken''
  userpw <- maybe (liftEither (Left NoSuchUser)) return (Map.lookup token users)
  password <- liftIO (T.putStrLn "Enter your password: " >> T.getLine)

  if userpw == password
     then return token
     else throwE WrongPassword

printResult'' :: Either LoginError Text -> IO ()
printResult'' result =
  T.putStrLn $ case result of
                 Right token -> append "Logged in with token: " token
                 Left InvalidEmail -> "Invalid email address entered."
                 Left NoSuchUser -> "No user with that email exists."
                 Left WrongPassword -> "Wrong password."

throwE :: e -> ExceptIO e a
throwE x = liftEither (Left x)

-- data ExceptIO e a = 
--   ExceptIO { runExceptIO :: IO (Either e a) }

catchE :: ExceptIO e a -> (e -> ExceptIO e a) -> ExceptIO e a
catchE throwing handler =
  ExceptIO $ do
    result <- runExceptIO throwing
    case result of 
      Left failure -> runExceptIO (handler failure)
      success -> return success

wrongPasswordHandler :: LoginError -> ExceptIO LoginError Text
wrongPasswordHandler WrongPassword = do
  liftIO (T.putStrLn "Wrong password, one more chance.")
  userLogin''
wrongPasswordHandler err = throwE err

printError :: LoginError -> ExceptIO LoginError a
printError err = do
  liftIO . T.putStrLn $ case err of
    WrongPassword -> "Wrong password. No more chances."
    NoSuchUser -> "No user with that email exists."
    InvalidEmail -> "Invalid email address entered."
  throwE err

loginDialogue :: ExceptIO LoginError ()
loginDialogue = do
  let retry = userLogin'' `catchE` wrongPasswordHandler
  token <- retry `catchE` printError
  liftIO $ T.putStrLn (append "Logged in with token: " token)




data A = A

