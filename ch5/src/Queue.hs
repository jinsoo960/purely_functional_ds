{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Queue where

class Queue q a | q -> a where
  qempty :: q
  qisEmpty :: q -> Bool

  qsnoc :: q -> a -> q
  qhead :: q -> Maybe a
  qtail :: q -> Maybe q
