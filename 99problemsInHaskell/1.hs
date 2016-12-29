-- 1) Find the last element of a list

lastE :: [a] -> a
lastE [] = error "No last element in the empty list"
lastE [x] = x
lastE (_:xs) = lastE xs


-- 2) Find the last but one element of a list.

secondLastE :: (Foldable f) => f a -> a
secondLastE = fst . foldl (\(a,b) x -> (b,x)) (errA, errB)
  where
    errA = error "List is empty"
    errB = error "List has only one value"


-- 3) Find the K'th element of a list. The first element of the list is at position 1

kthE :: (Ord k, Num k) => [a] -> k -> a
kthE (x:_) 1 = x
kthE [] _ = error "elem out of bounds"
kthE (x:xs) k
  | k <= 0 = error "elem out of bounds"
  | otherwise = kthE xs (k-1)


-- 4) Find the number of elements of a list
ll :: (Num n) => [a] -> n
ll = foldl (\n _ -> n+1) 0


-- 5) Reverse a list
rl :: [a] -> [a]
rl = foldl (flip (:)) []


-- 6) Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x).
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome xs = (head xs) == (last xs) && (isPalindrome $ init $ tail xs)


-- 7) Flatten a nested list structure
data NestedList a = Elem a | List [NestedList a] deriving (Show)
-- List [Elem 5,Elem 4,List [Elem 2,Elem 9],List [Elem 3,List [Elem 1]]]
flatten :: NestedList a -> [a]
flatten (Elem a) = [a]
flatten (List (x:xs)) = flatten x ++ flatten (List xs)
flatten (List []) = []


-- 8) Eliminate consecutive duplicates of list elements
compress :: (Eq a) => [a] -> [a]
-- compress x = foldr (\a l -> if (head l == a) then l else a:l) [last x] x

compress [] = []
compress [x] = [x]
compress (x:xs)
  | x == head xs = compress xs
  | otherwise = x : compress xs
