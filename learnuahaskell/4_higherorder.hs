-- currying
multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

multTwoWithNine = multThree 9

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

-- =============================================
-- higher order functions
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f(f(x))

zipwith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipwith' _ _ [] = []
zipwith' _ [] _ = []
zipwith' f (x:xs) (y:ys) = f x y : zipwith' f xs ys

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort (filter (<=x) xs)
        biggerSorted = quicksort (filter (>x) xs)
    in  smallerSorted ++ [x] ++ biggerSorted

-- find the largest number under 100,000 that's divisible by 3829
largestDivisibleBy3829 :: (Integral a) => a
largestDivisibleBy3829 = head(filter p [100000,99999..0])
  where p x = x `mod` 3829 == 0

-- find the sum of all odd squares that are smaller than 10,000
oddSum :: (Integral a) => a
oddSum = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))
-- or
oddSum' = sum (takeWhile (<10000) [x^2 | x <- [1..], odd (x^2)])

-- Collatz sequences
chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
  | even n = n: chain (n `div` 2)
  | odd n = n: chain (n*3+1)

-- for all starting numbers between 1 and 100, how many chains have a length greater than 15?
numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..100]))
  where isLong xs = length xs > 15

-- list of functions
listOfFuns = map (*) [0..]
-- (listOfFuns !! 4) 5

-- fold
sum' :: (Num a) => [a] -> a
-- sum' xs = foldl (\acc x -> acc + x) 0 xs
sum' = foldl (+) 0

elem' :: (Eq a) => a -> [a] -> Bool
elem' x = foldl (\acc n -> acc || (x == n)) False

map' :: (a->b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []
-- map' f = foldl (\acc x -> acc ++ [f x]) []
-- we should use foldr over foldl because ++ is more expensive operation as compared to :

maxList' :: (Ord a) => [a] -> a
maxList' = foldl1 max

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x:acc) []

product' :: (Num a) => [a] -> a
product' = foldl1 (*)

filter' :: (a -> Bool) -> [a] -> [a]
filter' f = foldr (\x acc -> if (f x) == True then x : acc else acc) []

-- How many elements does it take for the sum of the roots of all natural numbers to exceed 1000?
ex1 s = length (takeWhile (<=s) sumOfRoots) + 1
    where sumOfRoots = scanl1 (+) rootList
          rootList = map sqrt [1..]

--function application using $
-- map ($ 3) [(4+), (10*), (^2), sqrt]
