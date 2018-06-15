import Prelude hiding(read)

import qualified Data.Vector.Unboxed as VU
import Control.Monad.Loops
import Control.Monad.State
import Test.Hspec

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
main = hspec $ do
  describe "Sequential execution" $ do
    it "0 + 10 = 10" $ runState (runProgramSeq (add (IntPtr 0) 10)) (SeqState $ VU.fromList [0]) `shouldBe` (10, SeqState $ VU.fromList [10])
