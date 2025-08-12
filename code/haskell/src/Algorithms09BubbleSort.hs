{-# LANGUAGE NumericUnderscores #-}

module Main where

import System.CPUTime
import System.Random
import Text.Printf

{- | Perform Bubble Sort on a list.
Time complexity: O(n²)
Space complexity: O(1)
-}
bubbleSort ::
    -- | List to sort
    [Int] ->
    -- | Sorted list
    [Int]
bubbleSort xs = bubbleSortHelper (length xs) xs
  where
    bubbleSortHelper 0 ys = ys
    bubbleSortHelper n ys = bubbleSortHelper (n - 1) (bubblePass ys)

    bubblePass [] = []
    bubblePass [x] = [x]
    bubblePass (x : y : ys)
        | x > y = y : bubblePass (x : ys)
        | otherwise = x : bubblePass (y : ys)

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
    let nValues = [10_000, 20_000, 30_000, 40_000]

    mapM_ testBubbleSort nValues
  where
    testBubbleSort n = do
        setStdGen (mkStdGen 123) -- Fixed seed for reproducibility
        let arr = take n $ randomRs (0, n - 1) (mkStdGen 123)

        (sortedArr, timeSpent) <- timeAction $ return (bubbleSort arr)

        printf "Bubble Sort with N = %d\n" n
        if isSorted sortedArr
            then putStrLn "List is sorted."
            else putStrLn "List is NOT sorted."
        printf "Time taken: %f seconds\n\n" timeSpent
