{-# LANGUAGE NumericUnderscores #-}

module Main where

import System.CPUTime
import Text.Printf

{- | Perform linear search on a list.
Time complexity: O(n)
Space complexity: O(1)
-}
linearSearch ::
    -- | List to search in
    [Int] ->
    -- | Target value to search for
    Int ->
    -- | If found, Just index; otherwise, Nothing
    Maybe Int
linearSearch xs target = linearSearchHelper xs target 0
  where
    linearSearchHelper [] _ _ = Nothing
    linearSearchHelper (y : ys) x idx
        | y == x = Just idx
        | otherwise = linearSearchHelper ys x (idx + 1)

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

    mapM_ testLinearSearch nValues
  where
    testLinearSearch n = do
        let arr = [0 .. n - 1] -- Sequential elements
        let target = n - 1 -- Worst case: target at the end
        (result, timeSpent) <- timeAction $ return (linearSearch arr target)

        printf "Linear Search with N = %d\n" n
        case result of
            Just idx -> printf "Target found at index %d\n" idx
            Nothing -> putStrLn "Target not found"
        printf "Time taken: %f seconds\n\n" timeSpent
