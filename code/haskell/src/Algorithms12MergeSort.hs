{-# LANGUAGE NumericUnderscores #-}

module Main where

import System.CPUTime
import System.Random
import Text.Printf

{- | Merge two sorted lists.
Time complexity: O(n)
Space complexity: O(n)
-}
merge ::
    -- | First sorted list
    [Int] ->
    -- | Second sorted list
    [Int] ->
    -- | Merged sorted list
    [Int]
merge [] ys = ys
merge xs [] = xs
merge (x : xs) (y : ys)
    | x <= y = x : merge xs (y : ys)
    | otherwise = y : merge (x : xs) ys

{- | Perform Merge Sort on a list.
Time complexity: O(n log n)
Space complexity: O(n)
-}
mergeSort ::
    -- | List to sort
    [Int] ->
    -- | Sorted list
    [Int]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort left) (mergeSort right)
  where
    mid = length xs `div` 2
    (left, right) = splitAt mid xs

-- | Check if a list is sorted in ascending order.
isSorted :: [Int] -> Bool
isSorted [] = True
isSorted [_] = True
isSorted (x : y : xs) = x <= y && isSorted (y : xs)

-- | Measure execution time of an action in seconds.
timeAction :: IO a -> IO (a, Double)
timeAction action = do
    start <- getCPUTime
    result <- action
    end <- getCPUTime
    let time = fromIntegral (end - start) / 1e12
    return (result, time)

main :: IO ()
main = do
    let nValues = [100_000, 200_000, 400_000, 800_000]

    mapM_ testMergeSort nValues
  where
    testMergeSort n = do
        let gen = mkStdGen 123 -- Fixed seed for reproducibility
        let arr = take n $ randoms gen

        (sortedArr, timeSpent) <- timeAction $ return (mergeSort arr)

        printf "Merge Sort with N = %d\n" n
        if isSorted sortedArr
            then putStrLn "Array is sorted."
            else putStrLn "Array is NOT sorted."
        printf "Time taken: %f seconds\n\n" timeSpent
