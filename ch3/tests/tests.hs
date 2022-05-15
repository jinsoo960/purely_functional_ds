module Main where

import Test.Hspec
import Test.QuickCheck
import Heap
import BinaryHeap
import WeightBiasedHeap
import BinomialHeap
import qualified RedBlackTree
import Data.List (sort, nub)

main :: IO ()
main = hspec $ do
  describe "Heap" $ do 
    it "test BinaryHeap" $
      property (\s -> sort s == toSortedList (fromList s :: BinaryHeap Int))

    it "test WeightBiasedHeap" $
      property (\s -> sort s == toSortedList (fromList s :: WBHeap Int))

    it "test BinomialHeap" $
      property (\s -> sort s == toSortedList (fromList s :: BinomialHeap Int))

    it "test BinomialHeap" $
      property (\s -> sort s == toSortedList (fromList s :: BinomialHeap' Int))

    it "test BinomialHeap" $
      property (\s -> nub (sort s) == RedBlackTree.toSortedList (RedBlackTree.fromList s :: RedBlackTree.RedBlackTree Int))