module RedBlackTree where

data Color = R | B deriving (Eq, Show)
data Tree a = Empty | Tree Color (Tree a) a (Tree a) deriving (Eq, Show)

-- constraints
-- 1. no red node has a red child
-- 2. every path from the root to an empty node contains the same number of black nodes

-- suppose we have a tree T of size n
-- let k be the number of black nodes to an empty node
-- then if m is the longest path (or height), we must have m <= 2k since we cannot have 
-- more than two red node in a row in the path. so it must be at worst alternating
-- so, the depth of the tree is bound by 2k
-- well size of n >= 2^k since T the path to any leaf must be at least k long
-- which means it must contain a complete tree of height k
-- so 2 log n >= 2 k >= m. cool

member :: (Ord a) => a -> Tree a -> Bool
member _ Empty = False
member a (Tree _ l b r)
  | a < b = member a l 
  | a > b = member a r
  | otherwise = True 

-- how should insert work?
-- if we are inserting under a black node, then we should add it as a red
-- and the constraints are preserved
-- if we are inserting under a red node, we get a problem
-- we cannot make it red (1) and we cannot make it black (2)
-- insert it as a red node and fix up?
-- actually a red node must have both children or none
-- as if it only has one children, it must be black and the other being empty means
-- we contradict (1)

insert :: (Ord a) => a -> Tree a -> Tree a
insert x Empty = Tree R Empty x Empty
insert x t = let (Tree _ l y r) = helper x t in Tree B l y r
  where
    helper x Empty = Tree R Empty x Empty
    helper x t@(Tree c l y r)
      | x < y = balance c (helper x l) y r
      | x > y = balance c l y (helper x r)
      | otherwise = t

balance :: Color -> Tree a -> a -> Tree a -> Tree a
balance B (Tree R (Tree R a x b) y c) z d = Tree R (Tree B a x b) y (Tree B c z d)
balance B (Tree R a x (Tree R b y c)) z d = Tree R (Tree B a x b) y (Tree B c z d)
balance B a x (Tree R b y (Tree R c z d)) = Tree R (Tree B a x b) y (Tree B c z d)
balance B a x (Tree R (Tree R b y c) z d) = Tree R (Tree B a x b) y (Tree B c z d)
balance color a x b = Tree color a x b

-- linear time from sorted list 
-- since spliting takes linear time, this might not be exactly linear overall
-- can be fixed by using vector
fromOrdList :: (Ord a) => [a] -> Tree a
fromOrdList [] = Empty
fromOrdList [a] = Tree B Empty a Empty
fromOrdList [a, b] = Tree B (Tree R Empty a Empty) b Empty
fromOrdList [a, b, c] = Tree B (Tree R Empty a Empty) b (Tree R Empty c Empty)
fromOrdList as = if c /= c' then undefined else case c of 
  R -> Tree B left a right
  B -> Tree R left a right
  where
    n = length as
    (front, back') = splitAt (div n 2) as
    a = head back'
    back = tail back'
    -- check color for sanity but they should be same always
    left@(Tree c _ _ _) = fromOrdList front
    right@(Tree c' _ _ _) = fromOrdList back