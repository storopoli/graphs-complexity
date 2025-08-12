{-# LANGUAGE RankNTypes #-}

module Utils where

import System.CPUTime
import System.Random
import Text.Printf
import qualified Data.Map as Map
import qualified Data.Set as Set

-- | Graph represented as adjacency list
type Graph = Map.Map Int [Int]

-- | Create a graph from a list of edges
createGraph :: Int -> [(Int, Int)] -> Graph
createGraph numVertices =
    foldr addEdge initialGraph
  where
    initialGraph = Map.fromList [(i, []) | i <- [0..numVertices-1]]
    addEdge (u, v) graph =
        Map.adjust (v:) u $ Map.adjust (u:) v graph

-- | Create a chain graph (0-1-2-...-N-1)
createChainGraph :: Int -> Graph
createChainGraph n = createGraph n [(i, i+1) | i <- [0..n-2]]

-- | Generic test function for graph algorithms
testGraphAlgorithm ::
    String ->                           -- ^ Algorithm name
    (Graph -> Int -> Set.Set Int) ->    -- ^ Graph algorithm function
    [Int] ->                            -- ^ List of sizes to test
    IO ()
testGraphAlgorithm algName graphFn nValues = do
    mapM_ testGraph nValues
  where
    testGraph n = do
        let graph = createChainGraph n

        (visitedNodes, timeSpent) <- timeAction $ return (graphFn graph 0)

        printf "%s with N = %d\n" algName n
        printf "Visited %d nodes\n" (Set.size visitedNodes)
        printf "Time taken: %f seconds\n\n" timeSpent

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
