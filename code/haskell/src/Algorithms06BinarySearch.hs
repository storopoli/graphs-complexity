{-# LANGUAGE NumericUnderscores #-}

module Main where

import System.CPUTime
import Text.Printf

{- | Perform binary search on a sorted list.
Time complexity: O(log n)
Space complexity: O(1)
-}
binarySearch ::
    -- | Sorted list to search in
    [Int] ->
    -- | Target value to search for
    Int ->
    -- | If found, Just index; otherwise, Nothing
    Maybe Int
binarySearch xs target = binarySearchHelper 0 (length xs - 1)
  where
    binarySearchHelper l r
        | l > r = Nothing
        | xs !! mid == target = Just mid
        | xs !! mid < target = binarySearchHelper (mid + 1) r
        | otherwise = binarySearchHelper l (mid - 1)
      where
        mid = l + (r - l) `div` 2

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
    let nValues = [1_000_000, 2_000_000, 4_000_000, 8_000_000, 16_000_000]

    mapM_ testBinarySearch nValues
  where
    testBinarySearch n = do
        let arr = [0 .. n - 1] -- Sequential elements
        let target = n - 1 -- Target at the end
        (result, timeSpent) <- timeAction $ return (binarySearch arr target)

        printf "Binary Search with N = %d\n" n
        case result of
            Just idx -> printf "Target found at index %d\n" idx
            Nothing -> putStrLn "Target not found"
        printf "Time taken: %f seconds\n\n" timeSpent
