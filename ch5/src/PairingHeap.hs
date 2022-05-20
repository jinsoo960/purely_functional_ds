module PairingHeap where

data Heap a = EmptyHeap | Heap a [Heap a] 
  deriving (Eq, Show)

findMin :: (Ord a) => Heap a -> Maybe a
findMin EmptyHeap = Nothing
findMin (Heap a _) = Just a

merge :: (Ord a) => Heap a -> Heap a -> Heap a
merge EmptyHeap h = h
merge h EmptyHeap = h
merge h1@(Heap a1 c1) h2@(Heap a2 c2) =
  if a1 <= a2 then Heap a1 (h2:c1) else Heap a2 (h1:c2)

insert :: (Ord a) => a -> Heap a -> Heap a
insert a = merge (Heap a [])

mergePairs :: (Ord a) => [Heap a] -> Heap a
mergePairs [] = EmptyHeap
mergePairs [h] = h
mergePairs (h1:h2:hs) = merge (merge h1 h2) (mergePairs hs)

deleteMin :: (Ord a) => Heap a -> Maybe (Heap a)
deleteMin EmptyHeap = Nothing
deleteMin (Heap _ cs) = Just $ mergePairs cs


-- elements of the left subtree are greater or equal to the node element
data Tree a = EmptyTree | Tree (Tree a) a (Tree a)
  deriving (Eq, Show)

toBinary :: Heap a -> Tree a
toBinary EmptyHeap = EmptyTree
toBinary h = toBinaryHelper h []

-- h -> right siblings -> tree
-- such that leftmost child of h becomes left subtree of h 
-- and right sibling of h becomes right child of h
-- h is a child of something and we are maintaining that child is never empty
toBinaryHelper :: Heap a -> [Heap a] -> Tree a
toBinaryHelper EmptyHeap _ = undefined
toBinaryHelper (Heap a []) [] = Tree EmptyTree a EmptyTree
toBinaryHelper (Heap a []) (sib:sibs) = Tree EmptyTree a (toBinaryHelper sib sibs)
toBinaryHelper (Heap a (c:cs)) [] = Tree (toBinaryHelper c cs) a EmptyTree
toBinaryHelper (Heap a (c:cs)) (sib:sibs) = Tree (toBinaryHelper c cs) a (toBinaryHelper sib sibs)

findMin' :: (Ord a) => Tree a -> Maybe a
findMin' EmptyTree = Nothing
findMin' (Tree _ a _) = Just a

-- note that the right child of root is empty (r1 = r2 = [])
-- in the pairing heap implementation,
-- we make the merged subheap to be the left most child
merge' :: (Ord a) => Tree a -> Tree a -> Tree a
merge' EmptyTree t = t
merge' t EmptyTree = t
merge' (Tree l1 a1 r1) (Tree l2 a2 r2) =
  if  a1 <= a2 then Tree (Tree l2 a2 l1) a1 r1 else Tree (Tree l1 a1 l2) a2 r2

insert' :: (Ord a) => a -> Tree a -> Tree a
insert' a = merge' (Tree EmptyTree a EmptyTree)

deleteMin' :: (Ord a) => Tree a -> Maybe (Tree a)
deleteMin' EmptyTree = Nothing
deleteMin' (Tree l _ _) = Just $ mergePairs' l

mergePairs' :: (Ord a) => Tree a -> Tree a
mergePairs' EmptyTree = EmptyTree
mergePairs' t@(Tree l a EmptyTree) = t
mergePairs' (Tree l1 a (Tree l2 b r2)) =
  case r2 of
    EmptyTree -> merge' (Tree l1 a EmptyTree) (Tree l2 b EmptyTree)
    t -> merge' (merge' (Tree l1 a EmptyTree) (Tree l2 b EmptyTree)) (mergePairs' t)