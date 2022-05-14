{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, InstanceSigs #-}

module BinaryHeap where

import Heap

data BinaryHeap a = EmptyHeap | Node Int a (BinaryHeap a) (BinaryHeap a) deriving (Eq, Show)

instance Heap BinaryHeap where
  hempty = EmptyHeap
  hsingleton a = Node 1 a EmptyHeap EmptyHeap

  -- rank = right most path to emptyHeap
  -- in Node _ _ h1 h2, we always keep rank h1 >= rank h2
  merge h EmptyHeap = h
  merge EmptyHeap h = h
  merge h1@(Node r1 a1 lh1 rh1) h2@(Node r2 a2 lh2 rh2) 
    -- rank lh1 >= rank rh1
    -- if merge rh1 h2 gives correct structure
    -- makeNode will keep the rank correctly
    | a1 <= a2 = makeNode a1 lh1 (merge rh1 h2)
    | otherwise = makeNode a2 lh2 (merge h1 rh2)
  
  findMin :: (Ord a) => BinaryHeap a -> Maybe a
  findMin EmptyHeap = Nothing
  findMin (Node _ a _ _) = Just a 

  deleteMin :: (Ord a) => BinaryHeap a -> Maybe (BinaryHeap a)
  deleteMin EmptyHeap = Nothing
  deleteMin (Node _ a lh rh) = Just $ merge lh rh


insert' :: (Ord a) => a -> BinaryHeap a -> BinaryHeap a
insert' a EmptyHeap = Node 1 a EmptyHeap EmptyHeap
insert' a h@(Node r b lh rh) 
  | a <= b    = Node 1 a h EmptyHeap
  -- should insert go to the right or left?
  -- if we insert into right, then possibly leftist property will break
  -- consider when we only have singleton node
  -- so we should recurse into the right spine, but keep the rank consistent using
  -- the makeNode function
  | otherwise = makeNode b (insert a rh) lh

rank :: BinaryHeap a -> Int
rank EmptyHeap = 0
rank (Node r _ _ _) = r

makeNode :: a -> BinaryHeap a -> BinaryHeap a -> BinaryHeap a
makeNode a h1 h2 
  | r1 >= r2 = Node (r2 + 1) a h1 h2 
  | otherwise = Node (r1 + 1) a h2 h1
  where
    r1 = rank h1
    r2 = rank h2