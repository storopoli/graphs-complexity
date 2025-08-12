{-# LANGUAGE NumericUnderscores #-}

module Main where

import Utils

{- | Perform Selection Sort on a list.
Time complexity: O(n²)
Space complexity: O(1)
-}
selectionSort :: (Ord a) =>
    -- | List to sort
    [a] ->
    -- | Sorted list
    [a]
selectionSort [] = []
selectionSort xs = minElem : selectionSort remaining
  where
    minElem = minimum xs
    remaining = removeFirst minElem xs

    -- Remove the first occurrence of an element from a list
    removeFirst :: (Eq a) => a -> [a] -> [a]
    removeFirst _ [] = []
    removeFirst y (x:xs')
        | x == y = xs'
        | otherwise = x : removeFirst y xs'

main :: IO ()
main = testSortingAlgorithm "Selection Sort" selectionSort [10_000, 20_000, 30_000, 40_000]
