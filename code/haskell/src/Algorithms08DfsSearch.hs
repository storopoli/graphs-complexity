{-# LANGUAGE NumericUnderscores #-}

module Main where

import Utils
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)

{- | Depth-First Search algorithm.
Time complexity: O(V + E)
Space complexity: O(V)
-}
dfs :: Graph -> Int -> Set.Set Int
dfs graph startVertex = dfsHelper startVertex Set.empty
  where
    dfsHelper current visited
        | current `Set.member` visited = visited
        | otherwise =
            let newVisited = Set.insert current visited
                neighbors = fromMaybe [] (Map.lookup current graph)
                unvisitedNeighbors = filter (`Set.notMember` newVisited) neighbors
            in foldr dfsHelper newVisited unvisitedNeighbors

main :: IO ()
main = testGraphAlgorithm "DFS" dfs [1_000, 5_000, 10_000, 20_000, 30_000]
