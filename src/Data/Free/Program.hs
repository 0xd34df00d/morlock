{-# LANGUAGE GADTs #-}

module Data.Free.Program where

import Control.Monad

data Program l r where
  Pure :: r -> Program l r
  Bind :: l a -> (a -> Program l b) -> Program l b

lift :: l r -> Program l r
lift op = op `Bind` Pure

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
