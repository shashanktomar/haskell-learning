import ExprT
import Parser
import Expr

eval :: ExprT -> Integer
eval (Lit i) = i
eval (Mul a b) = (eval a) * (eval b)
eval (Add x y) = (eval x) + (eval y)

evalStr :: String -> Maybe Integer
evalStr = evalMaybe . p
  where
    p = parseExp Lit Add Mul
    evalMaybe Nothing = Nothing
    evalMaybe (Just e) = Just $ eval e

instance Expr ExprT where
  lit = Lit
  mul = Mul
  add = Add

instance Expr Integer where
  lit = id
  mul = (*)
  add = (+)

instance Expr Bool where
  lit = (>0)
  mul = (&&)
  add = (||)

newtype MinMax = MinMax Integer deriving (Eq, Show, Ord)

instance Expr MinMax where
  lit = MinMax
  mul = min
  add = max

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
  lit = Mod7 . (`mod`7)
  mul (Mod7 a) (Mod7 b) = lit $ a * b
  add (Mod7 a) (Mod7 b) = lit $ a + b

evalExp :: Expr a => String -> Maybe a
evalExp = parseExp lit add mul
