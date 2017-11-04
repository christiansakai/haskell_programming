{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Functor.Identity

-- 1
rDec :: Num a => Reader a a
rDec = ReaderT $ \a -> Identity (a - 1)

-- 2
rDec' :: Num a => Reader a a
rDec' = ReaderT $ Identity . (subtract 1)

-- 3 and 4
rShow :: Show a => ReaderT a Identity String
rShow = ReaderT $ Identity . show

-- 5
rPrintAndInc :: (Num a, Show a)
             => ReaderT a IO a
rPrintAndInc = ReaderT $ \a ->
  putStrLn ("Hi: " ++ (show a)) >>
    return (a + 1)

-- 6
sPrintIncAccum :: (Num a, Show a)
               => StateT a IO String
sPrintIncAccum = StateT $ \a ->
  putStrLn ("Hi: " ++ (show a)) >>
    return ((show a), a + 1)
