fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = (foldr f 1) . (filter even)
  where
    f = (*) . (2 `subtract`)

-- fun2 :: Integer -> Integer
-- fun2 1 = 0
-- fun2 n
--   | even n = n + fun2 (n `div` 2)
--   | otherwise = fun2 (3 * n + 1)

data Tree a = Leaf | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

isSmallerTree :: Tree a -> Tree a -> Bool
isSmallerTree Leaf _ = True
isSmallerTree _ Leaf = False
isSmallerTree (Node lh _ _ _) (Node rh _ _ _) = lh < rh

foldTree :: (Ord a) => [a] -> Tree a
foldTree = foldr (flip insert) Leaf
  where
    insert Leaf a = Node 0 Leaf a Leaf
    insert (Node h l n r) a
      | isSmallerTree l r = let t@(Node lh _ _ _) = (insert l a) in Node (lh+1) t n r
      | otherwise = let t@(Node rh _ _ _) = (insert r a) in Node (rh+1) l n t
