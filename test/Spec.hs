import Prelude hiding(read)

import qualified Data.Vector.Unboxed as VU
import Control.Monad.Loops
import Control.Monad.State

import Control.Concurrent.Morlock.Op
import Control.Concurrent.Morlock.State
import Control.Concurrent.Morlock.Run.Seq
import Data.Free.Program

add :: IntPtr -> Int -> Program Op Int
add p a = snd <$> do
  iterateUntil fst $ do
    value <- read p
    success <- cas p value (value + a)
    pure (success, value + a)

main :: IO ()
main = putStrLn "Test suite not yet implemented"
