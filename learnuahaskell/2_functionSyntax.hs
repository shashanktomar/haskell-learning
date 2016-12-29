factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial a = a * factorial (a-1)

first :: (a,b,c) -> a
first (x,_,_) = x
second :: (a,b,c) -> b
second (_,y,_) = y
third :: (a,b,c) -> c
third (_,_,z) = z

head' :: [a] -> a
head' [] = error "Can't fetch head of empty list"
head' (x:_) = x

length' :: (Num b) => [a] -> b
length' [] = 0
length' (x:xs) = 1 + length' xs

firstLetter :: String -> String
firstLetter "" = "Empty string, whoops!"
firstLetter all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
  | bmi <= 18.5 = "You are underweight, you should eat more"
  | bmi <= 25.0 = "You are normal"
  | bmi <= 30.0 = "You are fat"
  | otherwise = "You are a whale."
  where bmi = weight / (height ^ 2)

max' :: (Ord a) => a -> a -> a
max' a b
  | a > b = a
  | otherwise = b

mycompare :: (Ord a) => a -> a -> Ordering
a `mycompare` b
  | a > b     = GT
  | a == b    = EQ
  | otherwise = LT

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
  where (f:_) = firstname
        (l:_) = lastname

cylinderArea :: (RealFloat a) => a -> a -> a
cylinderArea r h =
  let sidearea = 2 * pi * r * h
      basearea = pi * r ^ 2
  in sidearea + 2 * basearea
