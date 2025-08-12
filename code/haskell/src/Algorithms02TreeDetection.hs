module Main where

import Data.Array
import qualified Data.Set as Set

-- | Graph represented as adjacency matrix
type AdjMatrix = Array (Int, Int) Int

-- | Create adjacency matrix from list of lists
createAdjMatrix :: [[Int]] -> AdjMatrix
createAdjMatrix matrix =
    let n = length matrix
     in array
            ((0, 0), (n - 1, n - 1))
            [((i, j), (matrix !! i) !! j) | i <- [0 .. n - 1], j <- [0 .. n - 1]]

-- | Get number of vertices in the graph
graphSize :: AdjMatrix -> Int
graphSize arr = let ((_, _), (n, _)) = bounds arr in n + 1

{- | Search for a cycle in a graph using DFS.
Time complexity: O(V + E)
Space complexity: O(V)
-}
searchCycle :: AdjMatrix -> Int -> Set.Set Int -> Int -> Bool
searchCycle graph vertex visited parent =
    let n = graphSize graph
        newVisited = Set.insert vertex visited
        neighbors = [i | i <- [0 .. n - 1], graph ! (vertex, i) == 1]
     in any (checkNeighbor newVisited) neighbors
  where
    checkNeighbor vis neighbor
        | neighbor `Set.notMember` vis = searchCycle graph neighbor vis vertex
        | neighbor /= parent = True -- Cycle detected (back edge to non-parent)
        | otherwise = False

{- | Check if a graph is a tree.
A graph is a tree if it is connected and has no cycles.
Time complexity: O(V + E)
Space complexity: O(V)
-}
isTree :: AdjMatrix -> Bool
isTree graph =
    let n = graphSize graph
        visited = Set.empty
        hasCycle = searchCycle graph 0 visited (-1)
        allVisited = dfsVisitAll graph 0 Set.empty
     in not hasCycle && Set.size allVisited == n

-- | Perform DFS to visit all reachable vertices
dfsVisitAll :: AdjMatrix -> Int -> Set.Set Int -> Set.Set Int
dfsVisitAll graph vertex visited
    | vertex `Set.member` visited = visited
    | otherwise =
        let n = graphSize graph
            newVisited = Set.insert vertex visited
            neighbors = [i | i <- [0 .. n - 1], graph ! (vertex, i) == 1, i `Set.notMember` newVisited]
         in foldl (flip (dfsVisitAll graph)) newVisited neighbors

-- | Test cases from the C implementation
testCases :: [([[Int]], String)]
testCases =
    [
        (
            [ [0, 1, 0, 1, 0]
            , [1, 0, 1, 0, 0]
            , [0, 1, 0, 0, 0]
            , [1, 0, 0, 0, 1]
            , [0, 0, 0, 1, 0]
            ]
        , "Test Case 1 (Tree)"
        )
    ,
        (
            [ [0, 1, 0, 1, 0]
            , [1, 0, 1, 1, 1]
            , [0, 1, 0, 0, 1]
            , [1, 1, 0, 0, 1]
            , [0, 1, 1, 1, 0]
            ]
        , "Test Case 2 (Not Tree)"
        )
    ]

-- | Test a single case
testCase :: ([[Int]], String) -> IO ()
testCase (matrix, name) = do
    putStrLn $ name ++ ":"
    let graph = createAdjMatrix matrix
    let result = isTree graph
    putStrLn $ "Is Tree? " ++ if result then "1" else "0"
    putStrLn ""

main :: IO ()
main = do
    putStrLn "Tree Detection Algorithm"
    putStrLn "========================"
    mapM_ testCase testCases
