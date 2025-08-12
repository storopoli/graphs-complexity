{-# LANGUAGE NumericUnderscores #-}

module Main where

import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Utils

{- | Breadth-First Search algorithm.
Time complexity: O(V + E)
Space complexity: O(V)
-}
bfs :: Graph -> Int -> Set.Set Int
bfs graph startVertex = bfsHelper [startVertex] Set.empty
  where
    bfsHelper [] visited = visited
    bfsHelper (current : queue) visited
        | current `Set.member` visited = bfsHelper queue visited
        | otherwise =
            let newVisited = Set.insert current visited
                neighbors = fromMaybe [] (Map.lookup current graph)
                unvisitedNeighbors = filter (`Set.notMember` newVisited) neighbors
                newQueue = queue ++ unvisitedNeighbors
             in bfsHelper newQueue newVisited

main :: IO ()
main = testGraphAlgorithm "BFS" bfs [1_000, 5_000, 10_000, 20_000, 30_000]
