{-# LANGUAGE FlexibleInstances #-}

import Expr
import qualified Data.Map.Strict as Map

class HasVars a where
  var :: String -> a

data VarExprT = Lit Integer
            | Var String
            | Add VarExprT VarExprT
            | Mul VarExprT VarExprT
  deriving (Show, Eq)

instance HasVars VarExprT where
  var = Var

instance Expr VarExprT where
  lit = Lit
  add = Add
  mul = Mul

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = flip M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit a = \_ -> Just a
  mul f1 f2 -> \m -> 
