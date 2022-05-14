{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, InstanceSigs #-}

module BinomialHeap where

import Heap

-- rank -> elt -> childeren
data Tree a = Node Int a [Tree a] deriving (Eq, Show)

-- link two nodes of same rank
-- smaller elt gets the root
link :: (Ord a) => Tree a -> Tree a -> Tree a
link h1@(Node r a1 c1) h2@(Node _ a2 c2) 
  | a1 <= a2 = Node (r + 1) a1 (h2 : c1)
  | otherwise = Node (r + 1) a2 (h1 : c2)

-- trees kept in increasing order of rank
newtype BinomialHeap a = BinomialHeap [Tree a] deriving (Eq, Show)

rank :: Tree a -> Int
rank (Node r _ _) = r

instance Heap BinomialHeap where
  hempty = BinomialHeap []
  hsingleton = BinomialHeap [Node 0 a []]

  insert a (BinomialHeap ts) = BinomialHeap $ insertTree (Node 0 a []) ts

  merge (BinomialHeap ts1) (BinomialHeap ts2) = BinomialHeap $ merge' ts1 ts2

  findMin (BinomialHeap ts) = root . fst <$> removeMinTree ts

  deleteMin (BinomialHeap []) = Nothing
  deleteMin (BinomialHeap ts) = do
    (Node _ _ cs, ts') <- removeMinTree ts
    return $ BinomialHeap (merge' (reverse cs) ts')


insertTree :: (Ord a) => Tree a -> [Tree a] -> [Tree a]
insertTree t [] = [t]
insertTree t ts@(t':ts') 
  | r < r' = t : ts
  | r == r' = insertTree (link t t') ts'
  | otherwise = t' : insertTree t ts'
  where 
    r = rank t
    r' = rank t'

merge' :: (Ord a) => [Tree a] -> [Tree a] -> [Tree a]
merge' [] h = h
merge' h [] = h
merge' ts1@(t1:ts1') ts2@(t2:ts2')
  | r1 == r2 = insertTree (link t1 t2) (merge' ts1' ts2')
  | r1 < r2 = t1 : merge' ts1' ts2
  | otherwise = t2 : merge' ts1 ts2'
  where
    r1 = rank t1
    r2 = rank t2 

root :: Tree a -> a
root (Node _ a _) = a

removeMinTree :: (Ord a) => [Tree a] -> Maybe (Tree a, [Tree a])
removeMinTree [] = Nothing
removeMinTree [t] = Just (t, [])
removeMinTree (t:ts) = 
  case removeMinTree ts of
    Nothing -> undefined -- should not happen
    Just (t', ts') -> if root t <= root t'
      then Just (t, ts)
      else Just (t', t : ts')