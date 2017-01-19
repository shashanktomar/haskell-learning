{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import Expr
import Parser
import StackVM

instance Expr Program where
  lit a = [PushI a]
  mul a b = a ++ b ++ [Mul]
  add a b = a ++ b ++ [Add]

compile :: String -> Maybe Program
compile s = parseExp lit add mul s
