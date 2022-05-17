{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies #-}
module BatchedQueue where

import Queue

-- first list contains elements in earliest to latest order
-- second list contains elements in the opposite order inserted later than the first list
data BatchedQueue a = BQueue [a] [a] deriving (Eq, Show) 

refillIfEmpty :: BatchedQueue a -> BatchedQueue a
refillIfEmpty (BQueue [] rs) = BQueue (reverse rs) []
refillIfEmpty bq = bq

instance Queue (BatchedQueue a) a where
  qempty = BQueue [] []

  qisEmpty (BQueue [] _) = True
  qisEmpty _ = False

  qsnoc (BQueue ls rs) a = refillIfEmpty (BQueue ls (a:rs))

  qhead (BQueue [] _) = Nothing
  qhead (BQueue (a:_) _) = Just a

  qtail (BQueue [] _) = Nothing
  qtail (BQueue (_:as) rs) = Just $ refillIfEmpty (BQueue as rs)


class Deque deq a | deq -> a where
  dempty :: deq
  disEmpty :: deq -> Bool

  dcons :: a -> deq -> deq
  dhead :: deq -> Maybe a
  dtail :: deq -> Maybe deq

  dsnoc :: deq -> a -> deq
  dlast :: deq -> Maybe a
  dinit :: deq -> Maybe deq


-- keep the invariant that if nonempty, then the first list should be nonempty
-- and we have >= 2 elements, both lists should be nonempty
balance :: BatchedQueue a -> BatchedQueue a
-- if as = [a], we get the splitAt -> ([], [a]) so BQueue [a] []
-- if as is longer, it works out
balance (BQueue [] as) = let (f, b) = splitAt (length as `div` 2) as in BQueue (reverse b) f
-- if as = [a], we get the splitAt -> ([a], []) so  so BQueue [a] []
-- if longer, no worries
balance (BQueue as []) = let (f, b) = splitAt ((length as + 1) `div` 2) as in BQueue f (reverse b)
balance bq = bq

instance Deque (BatchedQueue a) a where
  dempty = BQueue [] []

  disEmpty (BQueue [] []) = True
  disEmpty _ = False

  dcons a (BQueue ls rs) = balance (BQueue (a:ls) rs)
  dhead (BQueue [] _) = Nothing
  dhead (BQueue (a:_) _) = Just a
  dtail (BQueue [] _) = Nothing
  dtail (BQueue (_:ls) rs) = Just $ balance (BQueue ls rs)

  dsnoc (BQueue ls rs) a = balance (BQueue ls (a:rs))
  dlast (BQueue _ []) = Nothing
  dlast (BQueue _ (a:_)) = Just a
  dinit (BQueue [] _) = Nothing
  dinit (BQueue [_] []) = Just $ BQueue [] []
  dinit (BQueue ls (_:rs)) = Just $ balance (BQueue ls rs)
  dinit (BQueue ls []) = undefined -- should not happen since if ls >= 2, then rs >= 1