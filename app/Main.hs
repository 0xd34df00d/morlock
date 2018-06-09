import Prelude hiding(read)

import Control.Monad.Loops

newtype IntPtr = IntPtr { ptr :: Int } deriving (Eq, Ord, Show)

data Op r where
  OpPure :: a -> Op a
  OpBind :: Op a -> (a -> Op b) -> Op b

  OpRead :: IntPtr -> Op Int
  OpWrite :: IntPtr -> Int -> Op ()

  OpCAS :: IntPtr -> Int -> Int -> Op Bool

instance Functor Op where
  fmap f (OpPure x) = OpPure (f x)
  fmap f (OpBind op act) = do
    val <- op
    f <$> act val
  fmap f prim = do
    val <- prim
    pure $ f val

instance Applicative Op where
  pure = OpPure
  (OpPure f) <*> v = fmap f v
  (OpBind op act) <*> v = do
    val <- op
    f <- act val
    fmap f v

instance Monad Op where
  (>>=) = OpBind

read :: IntPtr -> Op Int
read = OpRead

cas :: IntPtr -> Int -> Int -> Op Bool
cas = OpCAS

add :: IntPtr -> Int -> Op Int
add p a = snd <$> do
  iterateUntil fst $ do
    value <- read p
    success <- cas p value (value + a)
    pure (success, value + a)
