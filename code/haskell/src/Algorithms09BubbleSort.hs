{-# LANGUAGE NumericUnderscores #-}

module Main where

import Utils

{- | Perform Bubble Sort on a list.
Time complexity: O(n²)
Space complexity: O(1)
-}
bubbleSort :: (Ord a) =>
    -- | List to sort
    [a] ->
    -- | Sorted list
    [a]
bubbleSort xs = bubbleSortHelper (length xs) xs
  where
    bubbleSortHelper 0 ys = ys
    bubbleSortHelper n ys = bubbleSortHelper (n - 1) (bubblePass ys)

    bubblePass :: (Ord a) => [a] -> [a]
    bubblePass [] = []
    bubblePass [x] = [x]
    bubblePass (x : y : ys)
        | x > y = y : bubblePass (x : ys)
        | otherwise = x : bubblePass (y : ys)

main :: IO ()
main = testSortingAlgorithm "Bubble Sort" bubbleSort [10_000, 20_000, 30_000, 40_000]
