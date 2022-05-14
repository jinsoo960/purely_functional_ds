module Heap where

class Heap heap where
  hempty :: (Ord a) => heap a
  hsingleton :: (Ord a) => a -> heap a
  insert :: (Ord a) => a -> heap a -> heap a
  insert a = merge (hsingleton a)
  merge :: (Ord a) => heap a -> heap a -> heap a
  findMin :: (Ord a) => heap a -> Maybe a
  deleteMin :: (Ord a) => heap a -> Maybe (heap a)

  -- why linear?
  -- at the first pass, we get n/2 merges of size 1 heaps
  -- at the second pass, we get n/4 merges of size 2 heaps 
  -- at the third pass, we get n/8 merges of size 4 heaps and so on
  -- \sum n/2^k log(2^{k - 1}) = n \sum (k - 1)/2^k \le 2n
  -- is there a reason why \sum n/2^n = 2? 
  -- consider \sum x^n/2^n for x < 2, = 1/(1-x/2)
  -- we differentiate that and evaluate at 1, then we get 2/(2 - x)^2 which is 2.
  -- nice.
  fromList :: (Ord a) => [a] -> heap a
  fromList = mergeUntilOne . map node
    where
      node a = hsingleton a
      -- mergePairs :: [heap a] -> [heap a] 
      mergePairs [] = []
      mergePairs [h] = [h]
      mergePairs (h1:h2:hs) = merge h1 h2 : mergePairs hs 

      -- we call this log n times since mergePairs decreases the length by half each time
      -- mergeUntilOne :: [heap a] -> heap a
      mergeUntilOne [] = hempty 
      mergeUntilOne [h] = h
      mergeUntilOne hs = mergeUntilOne $ mergePairs hs

  toSortedList :: (Ord a) => heap a -> [a]
  toSortedList h =
    case findMin h of
      Just a -> case deleteMin h of
        Just h' -> a : toSortedList h'
        Nothing -> [a]
      Nothing -> []
