{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, InstanceSigs #-}

module WeightBiasedHeap where

import Heap

data WBHeap a = EmptyHeap | Node Int a (WBHeap a) (WBHeap a) deriving (Eq, Show)


size :: WBHeap a -> Int
size EmptyHeap = 0
size (Node x _ _ _) = x

instance Heap WBHeap where
  hempty = EmptyHeap
  hsingleton a = Node 1 a EmptyHeap EmptyHeap

  merge EmptyHeap h = h
  merge h EmptyHeap = h
  merge h1@(Node x1 a1 lh1 rh1) h2@(Node x2 a2 lh2 rh2) 
    -- size lh1 >= size rh1
    -- if size h2 + size rh1 >= size lh1
    -- then we put it on the left 
    -- otherwise on the right?
    | a1 <= a2 = if x2 + size rh1 > size lh1
      then Node (x1 + x2) a1 (merge rh1 h2) lh1
      else Node (x1 + x2) a1 lh1 (merge rh1 h2)
    | otherwise = merge h2 h1

  findMin EmptyHeap = Nothing
  findMin (Node _ a _ _) = Just a 

  deleteMin EmptyHeap = Nothing
  deleteMin (Node _ a lh rh) = Just $ merge lh rh