{-# LANGUAGE GADTs #-}

module Main where

import Prelude hiding(read)

import Control.Monad.Loops

import Data.Free.Program

newtype IntPtr = IntPtr { cell :: Int } deriving (Eq, Ord, Show)

data Op r where
  Read  :: IntPtr -> Op Int
  Write :: IntPtr -> Int -> Op ()

  CAS :: IntPtr -> Int -> Int -> Op Bool

read :: IntPtr -> Program Op Int
read ptr = lift $ Read ptr

write :: IntPtr -> Int -> Program Op ()
write ptr val = lift $ Write ptr val

cas :: IntPtr -> Int -> Int -> Program Op Bool
cas ptr cmp val = lift $ CAS ptr cmp val

add :: IntPtr -> Int -> Program Op Int
add p a = snd <$> do
  iterateUntil fst $ do
    value <- read p
    success <- cas p value (value + a)
    pure (success, value + a)

main :: IO ()
main = undefined
