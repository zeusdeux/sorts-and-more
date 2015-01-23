-- binary tree
-- methods below mostly operate on binary *search* trees
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

-- inserts treating tree as a binary search tree
insertIntoTree :: (Eq a, Ord a) => a -> Tree a -> Tree a
insertIntoTree x Empty = Node x Empty Empty
insertIntoTree x (Node a left right)
  | x == a = Node a left right
  | x < a = Node a (insertIntoTree x left) right
  | otherwise = Node a left (insertIntoTree x right)

-- makes a binary search tree from list
makeTreeFromList :: (Ord a) => [a] -> Tree a
makeTreeFromList [] = Empty
makeTreeFromList xs = maker' xs Empty
  where
    maker' [] t     = t
    maker' (y:ys) t = maker' ys (insertIntoTree y t)

preorder :: Tree a -> [a]
preorder Empty = []
preorder (Node a left right) = [a] ++ preorder left ++ preorder right

inorder :: Tree a -> [a]
inorder Empty = []
inorder (Node a left right) = inorder left ++ [a] ++ inorder right

postorder :: Tree a -> [a]
postorder Empty = []
postorder (Node a left right) = postorder left ++ postorder right ++ [a]

-- bfs for binary tree using a queue to hold nodes to visit
-- http://web.cse.ohio-state.edu/~gurari/course/cis680/cis680Ch14.html#QQ1-46-92
bfs :: (Eq a) => a -> Tree a -> Maybe a
bfs _ Empty = Nothing
bfs x t = bfs' x [t]
  where
    bfs' :: (Eq a) => a -> [Tree a] -> Maybe a
    bfs' _ []                     = Nothing
    bfs' n (Empty:ys)             = bfs' n ys
    bfs' n ((Node tx l' r'):ys)
      | n == tx                   = Just tx
      | otherwise                 = bfs' n (ys ++ [l',r'])

-- dfs for a binary search tree
dfs :: (Eq a, Ord a) => a -> Tree a -> Maybe a
dfs _ Empty = Nothing
dfs x (Node a l r)
  | x == a = Just a
  | x < a = dfs x l
  | otherwise = dfs x r

-- generic dfs for binary trees using preorder traversal (root left right)
dfs' :: (Eq a) => a -> Tree a -> Maybe a
dfs' _ Empty = Nothing
dfs' x (Node a l r) = dfs'' x [l,r]
    where
      dfs'' :: (Eq a) => a -> [Tree a] -> Maybe a
      dfs'' _ [] = Nothing
      dfs'' n (Empty:ys) = dfs'' n ys
      dfs'' n ((Node b l' r'):ys)
        | n == b = Just b
        | otherwise = dfs'' n (l' : r' : ys)

-- joins two given binary search trees and rebalances em
joinTrees :: (Eq a, Ord a) => Tree a -> Tree a -> Tree a
joinTrees t1 Empty  = t1
joinTrees Empty t2  = t2
joinTrees t1@(Node a l r) t2@(Node b l' r')
  | a == b          = joinTrees t1 (deleteFromTree b t2)
  | b < a           = insertIntoTree a (joinTrees (joinTrees l t2) r)
  | b > a           = insertIntoTree a (joinTrees l (joinTrees r t2))

-- deletes a node from binary search tree
deleteFromTree :: (Eq a, Ord a) => a -> Tree a -> Tree a
deleteFromTree _ Empty = Empty
deleteFromTree n (Node a l r)
  | n == a    = joinTrees l r
  | n < a     = insertIntoTree a $ joinTrees (deleteFromTree n l) r
  | otherwise = insertIntoTree a $ joinTrees l (deleteFromTree n r)
