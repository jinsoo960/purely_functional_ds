{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module BatchedQueue where

import Queue

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
