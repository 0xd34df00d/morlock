{-# LANGUAGE GADTs #-}

module Control.Concurrent.Morlock.Op
  (
    Op(..),
    IntPtr(..),
    read,
    write,
    cas
  )where

import Prelude hiding(read)

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
