{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Queue where

class Queue q a | q -> a where
  empty :: q
  isEmpty :: q -> Bool

  snoc :: q -> a -> q
  head :: q -> Maybe a
  tail :: q -> Maybe q
