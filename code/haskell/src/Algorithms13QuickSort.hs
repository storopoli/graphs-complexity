{-# LANGUAGE NumericUnderscores #-}

module Main where

import Utils

{- | Perform Quick Sort on a list.
Average time complexity: O(n log n)
Worst case time complexity: O(n²)
Space complexity: O(log n) due to recursion
-}
quickSort :: (Ord a) =>
    -- | The list to sort
    [a] ->
    -- | The sorted list
    [a]
quickSort [] = []
quickSort [x] = [x]
quickSort (pivot : xs) = quickSort smaller ++ [pivot] ++ quickSort larger
  where
    smaller = [x | x <- xs, x <= pivot]
    larger = [x | x <- xs, x > pivot]

main :: IO ()
main = testSortingAlgorithm "Quick Sort" quickSort [100_000, 200_000, 400_000, 800_000]
