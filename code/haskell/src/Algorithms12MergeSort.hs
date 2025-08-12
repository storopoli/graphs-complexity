{-# LANGUAGE NumericUnderscores #-}

module Main where

import Utils

{- | Merge two sorted lists.
Time complexity: O(n)
Space complexity: O(n)
-}
merge :: (Ord a) =>
    -- | First sorted list
    [a] ->
    -- | Second sorted list
    [a] ->
    -- | Merged sorted list
    [a]
merge [] ys = ys
merge xs [] = xs
merge (x : xs) (y : ys)
    | x <= y = x : merge xs (y : ys)
    | otherwise = y : merge (x : xs) ys

{- | Perform Merge Sort on a list.
Time complexity: O(n log n)
Space complexity: O(n)
-}
mergeSort :: (Ord a) =>
    -- | List to sort
    [a] ->
    -- | Sorted list
    [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort left) (mergeSort right)
  where
    mid = length xs `div` 2
    (left, right) = splitAt mid xs

main :: IO ()
main = testSortingAlgorithm "Merge Sort" mergeSort [100_000, 200_000, 400_000, 800_000]
