module Golf where

skip :: (Integral n) => n -> [a] -> [a]
skip n = map snd . filter isIndex . (zip [1..])
  where
    isIndex = (==0) . (`mod`n) . fst

skips :: [a] -> [[a]]
skips xs = map ((flip skip) xs) [1..length xs]

localMaxima :: [Integer] -> [Integer]
localMaxima xs = fst $ foldr isMaxima ([], (last xs , last xs)) xs
  where
    isMaxima currentNum (list, (p1, p2))
      | currentNum < p1 && p2 < p1 = (p1:list, (currentNum, p1))
      | otherwise = (list, (currentNum, p1))

localMaxima' :: [Integer] -> [Integer]
localMaxima' = map (!! 1) . filter hasMaxima . groupList 3

-- [1,2,3,4,5,6] -> [[1,2,3],[2,3,4],[3,4,5],[4,5,6]]
-- groupList :: Int -> [a] -> [[a]]
-- groupList n (a:xs) =  filter ((==n) . length) ((a:(take (n-1) xs)): groupList n xs)
-- groupList _ _ = []

groupList :: Int -> [a] -> [[a]]
groupList n = foldr (group n) [[]]
  where
    group n a l@(x:xs)
      | n == 0 = [[]]
      | (length x) < n = (a:x):xs
      | otherwise = (a:(take (n-1) x)):l

hasMaxima :: [Integer] -> Bool
hasMaxima xs@(x:y:z:[]) = maximum xs == y
hasMaxima _ = False
