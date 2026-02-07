{-# LANGUAGE NumericUnderscores #-}

module Main where

import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Utils

{- | Breadth-First Search algorithm.
Time complexity: O(V + E)
Space complexity: O(V)
-}
bfs :: Graph -> Int -> Set.Set Int
bfs graph startVertex = bfsHelper (Seq.singleton startVertex) Set.empty
  where
    bfsHelper queue visited =
        case Seq.viewl queue of
            Seq.EmptyL -> visited
            current Seq.:< rest
                | current `Set.member` visited -> bfsHelper rest visited
                | otherwise ->
                    let newVisited = Set.insert current visited
                        neighbors = fromMaybe [] (Map.lookup current graph)
                        unvisitedNeighbors = filter (`Set.notMember` newVisited) neighbors
                        newQueue = foldl (Seq.|>) rest unvisitedNeighbors
                     in bfsHelper newQueue newVisited

main :: IO ()
main = testGraphAlgorithm "BFS" bfs [1_000, 5_000, 10_000, 20_000, 30_000]
