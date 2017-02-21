class TooMany a where
  tooMany :: a -> Bool

-- This will ask you to add FlexibleInstances support
-- instance TooMany (Int, String) where
--   tooMany (a,b) = a > 42 && b == "hello"

newtype StringAndNumber = StringAndNumber (Int, String) deriving (Eq, Show)

instance TooMany StringAndNumber where
  tooMany (StringAndNumber (a,b)) = a > 42 && b == "hello"

newtype IntInt = IntInt (Int, Int) deriving (Eq, Show)

instance TooMany IntInt where
  tooMany (IntInt (a,b)) = (a + b) > 42

newtype Weird a = Weird (a, a) deriving (Eq, Show)

instance (Num a, TooMany a) => TooMany (Weird a) where
  tooMany (Weird (a,b)) = tooMany (a + b)
