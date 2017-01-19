module Expr where

class Expr a where
  lit :: Integer -> a
  mul, add :: a -> a -> a
