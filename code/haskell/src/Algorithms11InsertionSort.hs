{-# LANGUAGE NumericUnderscores #-}

module Main where

import Utils

{- | Perform Insertion Sort on a list.
Time complexity: O(n²)
Space complexity: O(1)
-}
insertionSort :: (Ord a) =>
    -- | List to sort
    [a] ->
    -- | Sorted list
    [a]
insertionSort [] = []
insertionSort [x] = [x]
insertionSort (x:xs) = insert x (insertionSort xs)
  where
    -- Insert element in the correct position in a sorted list
    insert :: (Ord a) => a -> [a] -> [a]
    insert y [] = [y]
    insert y (z:zs)
        | y <= z = y : z : zs
        | otherwise = z : insert y zs

main :: IO ()
main = testSortingAlgorithm "Insertion Sort" insertionSort [10_000, 20_000, 30_000, 40_000]
