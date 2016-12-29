data BTree a = EmptyTree | Node a (BTree a) (BTree a) deriving (Show, Read, Eq)

singleton :: a -> BTree a
singleton a = Node a EmptyTree EmptyTree

insert :: (Ord a) => a -> BTree a -> BTree a
insert a EmptyTree = singleton a
insert a (Node n lt rt)
  | a == n = Node n lt rt
  | a < n = Node n (insert a lt) rt
  | otherwise = Node n lt (insert a rt)

-- foldr insert EmptyTree [2,5,1,6,3,9,8,4]

contains :: (Ord a) => a -> BTree a -> Bool
contains a EmptyTree = False
contains a (Node n lt rt)
  | a == n = True
  | a < n  = contains a lt
  | otherwise = contains a rt
