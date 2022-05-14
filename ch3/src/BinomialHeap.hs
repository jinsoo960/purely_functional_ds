module BinomialHeap where


-- rank -> elt -> childeren
data Tree a = Node Int a [Tree a] deriving (Eq, Show)

-- link two nodes of same rank
-- smaller elt gets the root
link :: (Ord a) => Tree a -> Tree a -> Tree a
link h1@(Node r a1 c1) h2@(Node _ a2 c2) 
  | a1 <= a2 = Node (r + 1) a1 (h2 : c1)
  | otherwise = Node (r + 1) a2 (h1 : c2)

-- trees kept in increasing order of rank
newtype Heap a = Heap [Tree a] deriving (Eq, Show)

rank :: Tree a -> Int
rank (Node r _ _) = r

insertTree :: (Ord a) => Tree a -> [Tree a] -> [Tree a]
insertTree t [] = [t]
insertTree t ts@(t':ts') 
  | r < r' = t : ts
  | r == r' = insertTree (link t t') ts'
  | otherwise = t' : insertTree t ts'
  where 
    r = rank t
    r' = rank t'

insert :: (Ord a) => a -> Heap a -> Heap a
insert a (Heap ts) = Heap $ insertTree (Node 0 a []) ts

merge :: (Ord a) => Heap a -> Heap a -> Heap a
merge (Heap ts1) (Heap ts2) = Heap $ merge' ts1 ts2

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

findMin :: (Ord a) => Heap a -> Maybe a
findMin (Heap ts) = root . fst <$> removeMinTree ts
  -- do
  -- (t, _) <- removeMinTree ts
  -- return $ root t
  -- case removeMinTree ts of
  --   Nothing -> Nothing
  --   Just (t, _) -> Just $ root t

deleteMin :: (Ord a) => Heap a -> Maybe (Heap a)
deleteMin (Heap []) = Nothing
deleteMin (Heap ts) = do
  (Node _ _ cs, ts') <- removeMinTree ts
  return $ Heap (merge' (reverse cs) ts')

  -- case removeMinTree ts of
  --   Nothing -> undefined
  --   Just ((Node _ _ cs), ts') -> Just $ Heap $ merge' (reverse cs) ts'

fromList :: (Ord a) => [a] -> Heap a 
fromList = mergeUntilOne . map node
  where
    node a = Heap [Node 0 a []]
    mergePairs :: (Ord a) => [Heap a] -> [Heap a]
    mergePairs [] = []
    mergePairs [h] = [h]
    mergePairs (h1:h2:hs) = merge h1 h2 : mergePairs hs 

    -- we call this log n times since mergePairs decreases the length by half each time
    mergeUntilOne :: (Ord a) => [Heap a] -> Heap a
    -- mergeUntilOne [] = EmptyHeap
    mergeUntilOne [h] = h
    mergeUntilOne hs = mergeUntilOne $ mergePairs hs
