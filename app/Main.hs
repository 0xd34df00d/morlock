{-# LANGUAGE GADTs #-}

module Main where

import Prelude hiding(read)

import qualified Data.Vector.Unboxed as VU

import Control.Monad.Loops
import Control.Monad.State hiding(lift)

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

data SeqState = SeqState
  { cells :: VU.Vector Int
  } deriving (Eq, Ord, Show)

readSeqSt :: IntPtr -> SeqState -> Int
readSeqSt ptr st = cells st VU.! cell ptr

writeSeqSt :: IntPtr -> Int -> SeqState -> SeqState
writeSeqSt ptr val st = st { cells = cells st VU.// [(cell ptr, val)] }

casSeqSt :: IntPtr -> Int -> Int -> SeqState -> (Bool, SeqState)
casSeqSt ptr cmp val st | cmp == curVal = (True, writeSeqSt ptr val st)
                        | otherwise = (False, st)
  where curVal = readSeqSt ptr st

runProgramSeq :: Program Op a -> State SeqState a
runProgramSeq (Pure x) = pure x
runProgramSeq (Bind (Read ptr) act) = do
  val <- gets $ readSeqSt ptr
  runProgramSeq $ act val
runProgramSeq (Bind (Write ptr val) act) = do
  modify' $ writeSeqSt ptr val
  runProgramSeq $ act ()
runProgramSeq (Bind (CAS ptr cmp val) act) = do
  res <- state $ casSeqSt ptr cmp val
  runProgramSeq $ act res

main :: IO ()
main = undefined
