{-# LANGUAGE NumericUnderscores #-}

module Main where

import System.CPUTime
import System.Random
import Text.Printf

{- | Perform Quick Sort on a list.
Average time complexity: O(n log n)
Worst case time complexity: O(n²)
Space complexity: O(log n) due to recursion
-}
quickSort ::
    -- | The list to sort
    [Int] ->
    -- | The sorted list
    [Int]
quickSort [] = []
quickSort [x] = [x]
quickSort (pivot : xs) = quickSort smaller ++ [pivot] ++ quickSort larger
  where
    smaller = [x | x <- xs, x <= pivot]
    larger = [x | x <- xs, x > pivot]

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

    mapM_ testQuickSort nValues
  where
    testQuickSort n = do
        let gen = mkStdGen 123 -- Fixed seed for reproducibility
        let arr = take n $ randoms gen

        (sortedArr, timeSpent) <- timeAction $ return (quickSort arr)

        printf "Quick Sort with N = %d\n" n
        if isSorted sortedArr
            then putStrLn "List is sorted."
            else putStrLn "List is NOT sorted."
        printf "Time taken: %f seconds\n\n" timeSpent
