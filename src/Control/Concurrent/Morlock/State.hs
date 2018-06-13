module Control.Concurrent.Morlock.State where

import qualified Data.Vector.Unboxed as VU

data SeqState = SeqState
  { cells :: VU.Vector Int
  } deriving (Eq, Ord, Show)
