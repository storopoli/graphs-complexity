{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Utils

{- | Counting Sort implementation using mutable arrays.
Works efficiently for arrays with a known range of values.
Requires elements to be in a reasonable integer range.
Time complexity: O(n + k) where k is the range of values
Space complexity: O(k)
-}
countingSort :: forall a. (Ord a, Enum a) => [a] -> [a]
countingSort [] = []
countingSort arr = runST $ do
    let minVal = minimum arr
        maxVal = maximum arr
        minIdx = fromEnum minVal
        maxIdx = fromEnum maxVal
        n = length arr
        k = maxIdx - minIdx + 1

    -- Create count array
    count <- newArray (0, k - 1) 0 :: ST s (STUArray s Int Int)

    -- Count occurrences (map values to 0-based indices)
    forM_ arr $ \x -> do
        let idx = fromEnum x - minIdx
        oldCount <- readArray count idx
        writeArray count idx (oldCount + 1)

    -- Transform count array to cumulative
    forM_ [1 .. k - 1] $ \i -> do
        prevCount <- readArray count (i - 1)
        currCount <- readArray count i
        writeArray count i (prevCount + currCount)

    -- Create output array
    output <- newArray (0, n - 1) minVal :: ST s (STArray s Int a)

    -- Build output array (traverse input in reverse for stability)
    forM_ (reverse arr) $ \x -> do
        let idx = fromEnum x - minIdx
        pos <- readArray count idx
        writeArray output (pos - 1) x
        writeArray count idx (pos - 1)

    -- Convert to list
    forM [0 .. n - 1] $ readArray output

main :: IO ()
main = testSortingAlgorithm "Counting Sort" countingSort [100_000, 200_000, 400_000, 800_000]
