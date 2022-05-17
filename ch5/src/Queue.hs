{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Queue where

class Queue q a | q -> a where
  isEmpty :: q -> Bool