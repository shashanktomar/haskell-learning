-- ============================================================
-- record syntax for creating access methods
type PhoneNumber = String
type Name = String
type PhoneBook = [(Name,PhoneNumber)]
data Person = Person { firstName :: Name
                     , lastName :: Name
                     , age :: Int
                     , height :: Float
                     , phoneNumber :: PhoneNumber
                   } deriving (Eq, Show, Read)

-- By using record syntax to create this data type, Haskell automatically made
-- these functions: firstName, lastName, age, height, phoneNumber. Try :t firstName
-- ============================================================

-- Type parameters
data Maybe a = Nothing | Just a
data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show)
data Vector a = Vector a a a deriving (Show)

-- ============================================================
-- Derived Instances

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
           deriving (Eq, Ord, Show, Read, Bounded, Enum)

-- ============================================================
-- Recursive DataStructures
infixr 5 :-:
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)
-- 3 :-: 4 :-: 5 :-: Empty

infixr 5  .++
(.++) :: List a -> List a -> List a
Empty .++ ys = ys
(x :-: xs) .++ ys = x :-: (xs .++ ys)
-- let a = 3 :-: 4 :-: 5 :-: Empty
-- let b = 6 :-: 7 :-: Empty
-- a .++ b

-- ============================================================
-- Typeclasses
-- class Eq a where
--     (==) :: a -> a -> Bool
--     (/=) :: a -> a -> Bool
--     x == y = not (x /= y)
--     x /= y = not (x == y)

data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where
    Red == Red = True
    Green == Green = True
    Yellow == Yellow = True
    _ == _ = False

instance Show TrafficLight where
    show Red = "Red light"
    show Yellow = "Yellow light"
    show Green = "Green light"

-- You can also make typeclasses that are subclasses of other typeclasses
-- class (Eq a) => Num a where
-- ...
-- So this is just like writing class Num a where, only we state that our type a must be an instance of Eq.
-- We're essentially saying that we have to make a type an instance of Eq before we can make
-- it an instance of Num

-- instance Eq (Maybe m) where
--     Just x == Just y = x == y
--     Nothing == Nothing = True
--     _ == _ = False
-- We had to add a class constraint! With this instance declaration,
-- we say this: we want all types of the form Maybe m to be part of
-- the Eq typeclass, but only those types where the m (so what's contained inside the Maybe)
-- is also a part of Eq.


-- ============================================================
