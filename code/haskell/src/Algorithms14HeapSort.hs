{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad (forM, forM_)
import Control.Monad.ST (ST, runST)
import Data.Array.MArray (newListArray, readArray, writeArray)
import Data.Array.ST (STArray)
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import Utils

{- | Perform Heap Sort on a list using mutable arrays.
Time complexity: O(n log n)
Space complexity: O(n)
-}
heapSort :: forall a. (Ord a) => [a] -> [a]
heapSort list = runST $ do
    let n = length list
    heap <- newListArray (1, n) list :: ST s (STArray s Int a)
    heapSizeRef <- newSTRef n
    let
        heapifyDown pos = do
            val <- readArray heap pos
            heapSize <- readSTRef heapSizeRef
            let children = filter (<= heapSize) [pos * 2, pos * 2 + 1]
            childrenVals <- forM children $ \i -> do
                childVal <- readArray heap i
                return (childVal, i)
            if null children
                then return ()
                else do
                    let (maxChildVal, maxChildIdx) = maximum childrenVals
                    if val >= maxChildVal
                        then return ()
                        else do
                            writeArray heap pos maxChildVal
                            writeArray heap maxChildIdx val
                            heapifyDown maxChildIdx
        lastParent = n `div` 2
    -- Build max heap
    forM_ [lastParent, lastParent - 1 .. 1] heapifyDown
    -- Extract elements one by one (in reverse order for ascending result)
    result <- forM [n, n - 1 .. 1] $ \i -> do
        top <- readArray heap 1
        val <- readArray heap i
        writeArray heap 1 val
        writeSTRef heapSizeRef (i - 1)
        heapifyDown 1
        return top
    return (reverse result)

main :: IO ()
main = testSortingAlgorithm "Heap Sort" heapSort [100_000, 200_000, 400_000, 800_000]
