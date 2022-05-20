module PairingHeap where

data Heap a = Empty | Heap a [Heap a] 
  deriving (Eq, Show)

findMin :: (Ord a) => Heap a -> Maybe a
findMin Empty = Nothing
findMin (Heap a _) = Just a

merge :: (Ord a) => Heap a -> Heap a -> Heap a
merge Empty h = h
merge h Empty = h
merge h1@(Heap a1 c1) h2@(Heap a2 c2) =
  if a1 <= a2 then Heap a1 (h2:c1) else Heap a2 (h1:c2)

insert :: (Ord a) => a -> Heap a -> Heap a
insert a = merge (Heap a [])

mergePairs :: (Ord a) => [Heap a] -> Heap a
mergePairs [] = Empty
mergePairs [h] = h
mergePairs (h1:h2:hs) = merge (merge h1 h2) (mergePairs hs)

deleteMin :: (Ord a) => Heap a -> Maybe (Heap a)
deleteMin Empty = Nothing
deleteMin (Heap _ cs) = Just $ mergePairs cs

