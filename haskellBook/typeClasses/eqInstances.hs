data TisAnInteger = TisAn Integer

instance Eq TisAnInteger where
  (TisAn x) == (TisAn y) = x == y

data TwoIntegers = Two Integer Integer

instance Eq TwoIntegers where
  (Two x y) == (Two a b) = x == a && y == b

data StringOrInt = TisAnInt Int
       | TisAString String

instance Eq StringOrInt where
  (TisAnInt x) == (TisAnInt y) = x == y
  (TisAString x) == (TisAString y) = x == y
  _ == _ = False

data Pair a = Pair a a

instance Eq a => Eq (Pair a) where
  (Pair x _) == (Pair a _) = x == a

data Tuple a b = Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  (Tuple a b) == (Tuple x y) = a == x && b == y

data Which a = ThisOne a | ThatOne a

instance Eq a => Eq (Which a) where
  (ThisOne a) == (ThisOne b) = a == b
  (ThatOne a) == (ThatOne b) = a == b
  _ == _ = False
