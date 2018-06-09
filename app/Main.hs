{-# LANGUAGE GADTs #-}

module Main where

import Prelude hiding(read)

import Control.Monad
import Control.Monad.Loops

newtype IntPtr = IntPtr Int deriving (Eq, Ord, Show)

data Op r where
  Read  :: IntPtr -> Op Int
  Write :: IntPtr -> Int -> Op ()

  CAS :: IntPtr -> Int -> Int -> Op Bool

data Program l r where
  Pure :: r -> Program l r
  Bind :: l a -> (a -> Program l b) -> Program l b

lift :: l r -> Program l r
lift op = op `Bind` Pure

read :: IntPtr -> Program Op Int
read ptr = lift $ Read ptr

write :: IntPtr -> Int -> Program Op ()
write ptr val = lift $ Write ptr val

cas :: IntPtr -> Int -> Int -> Program Op Bool
cas ptr cmp val = lift $ CAS ptr cmp val

instance Functor (Program l) where
  fmap f (Pure v) = Pure $ f v
  fmap f (Bind op act) = Bind op (fmap f . act)

instance Applicative (Program l) where
  pure = Pure

  Pure f    <*> v = f <$> v
  Bind op f <*> v = Bind op (\r -> f r <*> v)

instance Monad (Program l) where
  Pure v    >>= g = g v
  Bind op f >>= g = Bind op (f >=> g)

add :: IntPtr -> Int -> Program Op Int
add p a = snd <$> do
  iterateUntil fst $ do
    value <- read p
    success <- cas p value (value + a)
    pure (success, value + a)

main :: IO ()
main = undefined
