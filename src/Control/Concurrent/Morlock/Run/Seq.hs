{-# LANGUAGE GADTs #-}

module Control.Concurrent.Morlock.Run.Seq(runProgramSeq) where

import qualified Data.Vector.Unboxed as VU
import Control.Monad.State

import Control.Concurrent.Morlock.Op
import Control.Concurrent.Morlock.State
import Data.Free.Program

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
