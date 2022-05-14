{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module MyLib where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- This clearly only uses O(n) time
-- O(n) space is because suffix in the resulting list can be just 
-- pointers to the appropriate position in the input
suffixes :: [a] -> [[a]]
suffixes [] = [[]]
suffixes xs = xs : suffixes (tail xs)

data Tree a = EmptyTree | Node (Tree a) a (Tree a) 
  deriving (Eq, Show)

class Set set b | set -> b where
  emptySet :: set
  insertSet :: b -> set -> set
  memberSet :: b -> set -> Bool



insertTree :: (Ord a) => a -> Tree a -> Tree a
insertTree a EmptyTree = Node EmptyTree a EmptyTree
insertTree a (Node l b r) 
  | a == b = Node l b r
  | a < b = Node (insertTree a l) b r
  | otherwise = Node l b (insertTree a r)

memberTree :: (Ord a) => a -> Tree a -> Bool
memberTree a EmptyTree = False
memberTree a (Node l b r) 
  | a == b = True
  | a < b = memberTree a l
  | otherwise = memberTree a r

-- Can we prove this is correct?
-- Well, if exists, candidate >= a
-- is candidate the closest to a that we have ever seen?
-- Suppose we write the elements that we see as a list
-- [b_0, b_1, ..., b_k]
-- candidate is the last b_i such that b_i <= a 
-- Suppose there is b_j such that b_i < b_j <= a and j < i
-- b_i is in the left subtree of b_j
-- but then we must have traveled to the left subtree at b_j's step to meet b_i
-- evenetually, but that means a < b_j. contradiction.
-- So the candidate is the closest to a we see at each stage.
memberTree' :: (Ord a) => a -> Tree a -> Bool
memberTree' = mtHelper Nothing

mtHelper :: (Ord a) => Maybe a -> a -> Tree a -> Bool
mtHelper Nothing a EmptyTree = False
mtHelper (Just b) a EmptyTree = a == b
mtHelper candidate a (Node l b r) 
  -- if a < b, b cannot be candidate as there is just no hope
  -- we already know a != b
  | a < b = mtHelper candidate a l
  | otherwise = mtHelper (Just b) a r

instance (Ord a) => Set (Tree a) a where
  emptySet = EmptyTree
  insertSet = insertTree
  memberSet = memberTree

completeBTree :: a -> Int -> Tree a
completeBTree _ 0 = EmptyTree
completeBTree a n = let cbt = completeBTree a (n - 1) in Node cbt a cbt

-- n - 1 = 2m
-- (m + 1 + m, m + 1 + m + 1) = (2m + 1, 2m + 2) = (n, n + 1)
-- n - 1 = 2m + 1, n = 2m + 2
-- (m + 1 + m + 1, m + 1 + 1 + m + 1) = (2m + 2, 2m + 3)
create2 :: a -> Int -> (Tree a, Tree a)
create2 a 0 = (EmptyTree, Node EmptyTree a EmptyTree)
create2 a n
  | q == 0 = let (bt1, bt2) = create2 a m in (Node bt1 a bt1, Node bt1 a bt2)
  | otherwise = let (bt1, bt2) = create2 a m in (Node bt1 a bt2, Node bt2 a bt2)
  where (m, q) = divMod (n - 1) 2

createBalanced :: a -> Int -> Tree a
createBalanced _ 0 = EmptyTree
createBalanced a n 
  | q == 0 = let bt = createBalanced a m in Node bt a bt
  | otherwise = let (bt1, bt2) = create2 a m in Node bt1 a bt2 
  where (m, q) = divMod (n - 1) 2

-- To count how many unique trees are made

createBalanced' :: Int -> Int -> Tree Int
createBalanced' _ 0 = EmptyTree
createBalanced' a n 
  | q == 0 = let bt = createBalanced' (a + 1) m in Node bt a bt
  | otherwise = let (bt1, bt2) = create2' (a + 1) m in Node bt1 a bt2 
  where 
    (m, q) = divMod (n - 1) 2
    create2' :: Int -> Int -> (Tree Int, Tree Int)
    create2' a 0 = (EmptyTree, Node EmptyTree a EmptyTree)
    create2' a n
      | q == 0 = let (bt1, bt2) = create2' (a + 2) m in (Node bt1 a bt1, Node bt1 (a + 1) bt2)
      | otherwise = let (bt1, bt2) = create2' (a + 2) m in (Node bt1 a bt2, Node bt2 (a + 1) bt2)
      where (m, q) = divMod (n - 1) 2


class FiniteMap map key val | map -> key, map -> val where
  emptyMap :: map
  bindMap :: key -> val -> map -> map
  lookupMap :: key -> map -> Maybe val

instance (Ord a) => FiniteMap (Tree (a, b)) a b where
  emptyMap = EmptyTree

  bindMap k v EmptyTree = Node EmptyTree (k, v) EmptyTree
  bindMap k v (Node l kv@(k', _) r) 
    | k == k' = Node l (k, v) r
    | k < k' = Node (bindMap k v l) kv r
    | otherwise = Node l kv (bindMap k v r)

  lookupMap a EmptyTree = Nothing
  lookupMap k (Node l (k', v') r) 
    | k == k' = Just v'
    | k < k' = lookupMap k l
    | otherwise = lookupMap k r
