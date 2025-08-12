{-# LANGUAGE NumericUnderscores #-}

module Main where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)
import System.CPUTime
import Text.Printf

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

{- | Breadth-First Search algorithm.
Time complexity: O(V + E)
Space complexity: O(V)
-}
bfs :: Graph -> Int -> Set.Set Int
bfs graph startVertex = bfsHelper [startVertex] Set.empty
  where
    bfsHelper [] visited = visited
    bfsHelper (current:queue) visited
        | current `Set.member` visited = bfsHelper queue visited
        | otherwise =
            let newVisited = Set.insert current visited
                neighbors = fromMaybe [] (Map.lookup current graph)
                unvisitedNeighbors = filter (`Set.notMember` newVisited) neighbors
                newQueue = queue ++ unvisitedNeighbors
            in bfsHelper newQueue newVisited

-- | Create a chain graph (0-1-2-...-N-1)
createChainGraph :: Int -> Graph
createChainGraph n = createGraph n [(i, i+1) | i <- [0..n-2]]

-- | Measure execution time of an action
timeAction :: IO a -> IO (a, Double)
timeAction action = do
    start <- getCPUTime
    result <- action
    end <- getCPUTime
    let time = fromIntegral (end - start) / 1e12
    return (result, time)

-- | Test BFS with different graph sizes
testBfs :: Int -> IO ()
testBfs n = do
    let graph = createChainGraph n

    (visitedNodes, timeSpent) <- timeAction $ return (bfs graph 0)

    printf "BFS with N = %d\n" n
    printf "Visited %d nodes\n" (Set.size visitedNodes)
    printf "Time taken: %f seconds\n\n" timeSpent

main :: IO ()
main = do
    let nValues = [1_000, 5_000, 10_000, 20_000, 30_000]
    mapM_ testBfs nValues
