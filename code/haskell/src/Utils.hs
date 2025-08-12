{-# LANGUAGE RankNTypes #-}

module Utils where

import System.CPUTime
import System.Random
import Text.Printf

-- | Check if a list is sorted in ascending order
isSorted :: (Ord a) => [a] -> Bool
isSorted [] = True
isSorted [_] = True
isSorted (x:y:xs) = x <= y && isSorted (y:xs)

-- | Measure execution time of an action in seconds
timeAction :: IO a -> IO (a, Double)
timeAction action = do
    start <- getCPUTime
    result <- action
    end <- getCPUTime
    let time = fromIntegral (end - start) / 1e12
    return (result, time)

-- | Generic test function for sorting algorithms
testSortingAlgorithm ::
    String ->                    -- ^ Algorithm name
    (forall a. Ord a => [a] -> [a]) -> -- ^ Polymorphic sorting function
    [Int] ->                     -- ^ List of sizes to test
    IO ()
testSortingAlgorithm algName sortFn nValues = do
    mapM_ testSort nValues
  where
    testSort n = do
        let gen = mkStdGen 123  -- Fixed seed for reproducibility
        let arr = take n $ randomRs (0, n-1) gen :: [Int]

        (sortedArr, timeSpent) <- timeAction $ return (sortFn arr)

        printf "%s with N = %d\n" algName n
        if isSorted sortedArr
            then putStrLn "List is sorted."
            else putStrLn "List is NOT sorted."
        printf "Time taken: %f seconds\n\n" timeSpent
