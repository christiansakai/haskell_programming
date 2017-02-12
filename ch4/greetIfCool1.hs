module GreetIfCool1 where

greetIfCool :: String -> IO ()
greetIfCool coolness = 
  if cool
     then putStrLn "Eyyy. What's shakin?"
     else putStrLn "psssh."
  where cool = coolness == "downright frosty yo"
