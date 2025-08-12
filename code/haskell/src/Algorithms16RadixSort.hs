{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Utils

-- | Base for radix sort (decimal)
base :: Int
base = 10

{- | Radix Sort implementation using counting sort for each digit.
Works with types that can be converted to integers via Enum.
Time complexity: O(d * (n + k)) where d is number of digits, k is base
Space complexity: O(n + k)
-}
radixSort :: forall a. (Ord a, Enum a) => [a] -> [a]
radixSort [] = []
radixSort arr =
    let maxVal = fromEnum $ maximum arr
        maxDigits = length $ show maxVal
    in foldl countingSortByDigit arr [10^i | i <- [0..maxDigits-1]]

-- | Counting sort for a specific digit position
countingSortByDigit :: forall a. (Ord a, Enum a) => [a] -> Int -> [a]
countingSortByDigit arr expVal = runST $ do
    let n = length arr
    
    -- Create count array for base 10
    count <- newArray (0, base-1) 0 :: ST s (STUArray s Int Int)
    
    -- Count occurrences of each digit
    forM_ arr $ \x -> do
        let digit = (fromEnum x `div` expVal) `mod` base
        oldCount <- readArray count digit
        writeArray count digit (oldCount + 1)
    
    -- Convert to cumulative counts
    forM_ [1..base-1] $ \i -> do
        prevCount <- readArray count (i-1)
        currCount <- readArray count i
        writeArray count i (prevCount + currCount)
    
    -- Create output array
    let minVal = minimum arr
    output <- newArray (0, n-1) minVal :: ST s (STArray s Int a)
    
    -- Build output array (traverse in reverse for stability)
    forM_ (reverse arr) $ \x -> do
        let digit = (fromEnum x `div` expVal) `mod` base
        pos <- readArray count digit
        writeArray output (pos - 1) x
        writeArray count digit (pos - 1)
    
    -- Convert to list
    forM [0..n-1] $ readArray output

main :: IO ()
main = testSortingAlgorithm "Radix Sort" radixSort [100_000, 200_000, 400_000, 800_000]