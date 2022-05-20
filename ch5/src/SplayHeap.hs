module SplayHeap where

data Tree a = Empty | Tree (Tree a) a (Tree a)
  deriving (Eq, Show)

insert :: (Ord a) => a -> Tree a -> Tree a
insert a t = Tree (smallerEq a t) a (bigger a t)

-- give a new tree containing the elements of tree bigger than pivot
-- rotate the tree when we follow two left branch in a row
bigger :: (Ord a) => a -> Tree a -> Tree a
bigger _ Empty = Empty
bigger pivot (Tree l x r) = 
  if x <= pivot then bigger pivot r
  else case l of
    Empty -> Tree Empty x r
    Tree l' y r' -> 
      if y <= pivot then Tree (bigger pivot r') x r
      else Tree (bigger pivot l') y (Tree r' x r)

-- give a new tree containing the elements of tree smaller than or equal to pivot
smallerEq :: (Ord a) => a -> Tree a -> Tree a
smallerEq _ Empty = Empty
smallerEq pivot (Tree l x r) =
  if x > pivot then smallerEq pivot l
  -- elts in l <= x <= pivot 
  else case r of
    Empty -> Tree l x Empty
    Tree l' y r' ->
      -- pivot < y <= elts in r'
      -- so we only need to look at the l'
      if y > pivot then Tree l x (smallerEq pivot l')
      -- y <= pivot so l' is ok but we need to look at r' 
      -- also, x < l' <= y < r'
      else Tree (Tree l x l') y (smallerEq pivot r')

findMin :: (Ord a) => Tree a -> Maybe a
findMin Empty = Nothing
findMin (Tree Empty x _) = Just x
findMin (Tree l _ _) = findMin l

deleteMin :: (Ord a) => Tree a -> Maybe (Tree a)
deleteMin Empty = Nothing
deleteMin (Tree Empty _ r) = Just r
deleteMin (Tree (Tree Empty _ r') y r) = Just $ Tree r' y r
deleteMin (Tree (Tree l' x r') y r) = do
  t <- deleteMin l'
  return $ Tree t x (Tree r' y r)

-- amortized cost of partitioning, (smallerEq, bigger) called simultaneously
-- since their search path is same, zig-zag case
-- x > pivot and y <= pivot
-- s =    x
--       / \
-- t =  y   d
--     / \
--    c   u
-- becomes
-- s' =  y    t' = x
--      / \       / \
--     c   u1    u2  d
-- where (u1, u2) = partition u
-- A(s) = T(s) + \Phi(s') + \Phi(t') - \Phi(s) 
-- = 1 + T(u) + \Phi(s') + \Phi(t') - \Phi(s)
-- = 1 + A(u) - \Phi(u1) - \Phi(u2) + \Phi(u) + \Phi(s') + \Phi(t') - \Phi(s) 
-- = 1 + A(u) - \Phi(u1) - \Phi(u2) + \Phi(u) + \phi(s') + \Phi(c) + \Phi(u1) + \phi(t') + \Phi(u2) + \Phi(d) - \Phi(s) 
-- = 1 + A(u) + \Phi(u) + \phi(s') + \phi(t') + \Phi(c) + \Phi(d) - \Phi(s)
-- = 1 + A(u) + \Phi(u) + \phi(s') + \phi(t') + \Phi(c) + \Phi(d) - \phi(s) - \phi(t) - \Phi(d) - \Phi(c) - \Phi(u) 
-- = 1 + A(u) + \phi(s') + \phi(t') - \phi(s) - \phi(t)
-- < 2 + 2 \phi(u) + \phi(s') + \phi(t') - \phi(s) - \phi(t)
-- < 2 + \phi(u) + \phi(s') + \phi(t') - \phi(s) 
-- #s' + #t' < #s so 1 + \phi(s') + \phi(t') < 2 \phi(s)
-- < 1 + \phi(u) + \phi(s)
-- < 1 + 2 \phi(s)

-- now deleteMin
-- s =    x
--       / \
-- t =  y   d
--     / \
--    u   c
-- becomes, where u' = deleteMin u
--       y      = s'
--      / \
--     u'  x    = t'
--        / \
--       c   d
-- A(s) = T(s) + \Phi(s') - \Phi(s)
-- = 1 + T(u) + \Phi(s') - \Phi(s)
-- = 1 + A(u) - \Phi(u') + \Phi(u) + \Phi(s') - \Phi(s)
-- = 1 + A(u) - \Phi(u') + \Phi(u) - \phi(s) - \phi(t) - \Phi(d) - \Phi(u) - \Phi(c) 
--   + phi(s') + Phi(u') + \phi(t') + \Phi(c) + \Phi(d)
-- = 1 + A(u) - \phi(s) + \phi(s') - \phi(t) + \phi(t')
-- since \phi(s') < \phi(s)
-- < 1 + A(u) - \phi(t) + \phi(t')
-- assume A(u) < 1 + 2 \phi(u)
-- < 2 + 2 \phi(u) - \phi(t) + \phi(t')
-- since \phi(u) < \phi(t)
-- < 2 + \phi(u) + \phi(t')
-- note #u + #t' = #u + 1 + #c + #d < 2 + #d + #c + #u = #s
-- < 1 + 2 \phi(s)

fromList :: (Ord a) => [a] -> Tree a
fromList = foldr insert Empty

inorderTraverse :: Tree a -> [a]
inorderTraverse Empty = []
inorderTraverse (Tree l x r) = inorderTraverse l ++ [x] ++ inorderTraverse r

sortSplayHeap :: (Ord a) => [a] -> [a]
sortSplayHeap = inorderTraverse . fromList

-- what is the runtime of sortSplayHeap on already ordered list?
-- suppose increasing order
-- then, we will always have empty right branch
-- suppose for induction that is the case T(l x Empty)
-- if we insert y > x, we will get T(T(l x Empty) y Empty) and so on...
-- so we essentially get a linear tree, but each insert will take constant time