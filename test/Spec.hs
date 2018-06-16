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

initState :: SeqState
initState = SeqState $ VU.fromList [0]

initState' :: [Int] -> SeqState
initState' = SeqState . VU.fromList

runProgramSeq' p = runState $ runProgramSeq p

main :: IO ()
main = hspec $ do
  describe "Sequential execution" $ do
    it "basically works (0 + 10 = 10)" $ runProgramSeq' (add (IntPtr 0) 10) initState `shouldBe` (10, SeqState $ VU.fromList [10])
    it "composes linearly (0 + 10 + 10 = 20)" $ runProgramSeq' (add (IntPtr 0) 10 >> add (IntPtr 0) 10) initState `shouldBe` (20, SeqState $ VU.fromList [20])
    it "composes in parallel (0 + 10 = 10; 0 + 10 = 10)" $ runProgramSeq' (add (IntPtr 0) 10 >> add (IntPtr 1) 10) (initState' [0, 0]) `shouldBe` (10, SeqState $ VU.fromList [10, 10])
