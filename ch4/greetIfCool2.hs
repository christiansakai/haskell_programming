module GreetIfCool2 where

greetIfCool :: String -> IO ()
greetIfCool = 
  if cool coolness
     then putStrLn "Eyyy. What's shakin?"
     else putStrLn "psssh."
       where cool v = v == "downright frosty yo"
