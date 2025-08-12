module Main where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (find)

-- | Graph represented as adjacency map with edge multiplicity
type EdgeGraph = Map.Map Int (Map.Map Int Int)

-- | Create graph from adjacency matrix
createEdgeGraph :: [[Int]] -> EdgeGraph
createEdgeGraph matrix =
    Map.fromList $ zipWith createVertex [0..] matrix
  where
    createVertex i row =
        (i, Map.fromList [(j, count) | (j, count) <- zip [0..] row, count > 0])

-- | Get degree of a vertex (sum of all edge multiplicities)
degree :: EdgeGraph -> Int -> Int
degree graph vertex =
    case Map.lookup vertex graph of
        Nothing -> 0
        Just neighbors -> sum (Map.elems neighbors)

-- | Check if edge exists and remove it
removeEdge :: EdgeGraph -> Int -> Int -> EdgeGraph
removeEdge graph u v =
    let graph' = updateVertex u v graph
        graph'' = updateVertex v u graph'
    in graph''
  where
    updateVertex from to  =
        Map.adjust (Map.update (\count -> if count > 1 then Just (count - 1) else Nothing) to) from

-- | Find any neighbor of a vertex
findNeighbor :: EdgeGraph -> Int -> Maybe Int
findNeighbor graph vertex =
    case Map.lookup vertex graph of
        Nothing -> Nothing
        Just neighbors ->
            case Map.toList neighbors of
                [] -> Nothing
                (neighbor, _):_ -> Just neighbor

{- | Find Eulerian path in a graph.
Time complexity: O(E) where E is number of edges
Space complexity: O(V + E)
-}
findEulerianPath :: EdgeGraph -> [Int]
findEulerianPath graph
    | not (hasEulerianPath graph) = []
    | otherwise = hierholzersAlgorithm graph startVertex
  where
    vertices = Map.keys graph
    oddDegreeVertices = filter (\v -> degree graph v `mod` 2 == 1) vertices
    startVertex = case oddDegreeVertices of
        [] -> head vertices  -- Eulerian cycle - start anywhere
        (v:_) -> v          -- Eulerian path - start at odd degree vertex

-- | Check if graph has Eulerian path (0 or 2 vertices with odd degree)
hasEulerianPath :: EdgeGraph -> Bool
hasEulerianPath graph =
    let vertices = Map.keys graph
        oddDegreeCount = length $ filter (\v -> degree graph v `mod` 2 == 1) vertices
    in oddDegreeCount == 0 || oddDegreeCount == 2

-- | Hierholzer's algorithm for finding Eulerian path
hierholzersAlgorithm :: EdgeGraph -> Int -> [Int]
hierholzersAlgorithm graph start = reverse $ findPath graph [start] []
  where
    findPath g stack path
        | null stack = path
        | otherwise =
            let current = head stack
                rest = tail stack
            in case findNeighbor g current of
                Nothing -> findPath g rest (current : path)
                Just neighbor ->
                    let g' = removeEdge g current neighbor
                    in findPath g' (neighbor : stack) path

-- | Test cases from the C implementation
testCases :: [([[Int]], String)]
testCases =
    [ ( [[0,1,0,0,1],
         [1,0,1,1,0],
         [0,1,0,1,0],
         [0,1,1,0,0],
         [1,0,0,0,0]]
      , "Test Case 1" )
    , ( [[0,1,0,1,1],
         [1,0,1,0,1],
         [0,1,0,1,1],
         [1,1,1,0,0],
         [1,0,1,0,0]]
      , "Test Case 2" )
    , ( [[0,1,0,0,1],
         [1,0,1,1,1],
         [0,1,0,1,0],
         [0,1,1,0,1],
         [1,1,0,1,0]]
      , "Test Case 3" )
    ]

-- | Test a single case
testCase :: ([[Int]], String) -> IO ()
testCase (matrix, name) = do
    putStrLn $ name ++ ":"
    let graph = createEdgeGraph matrix
    let path = findEulerianPath graph
    if null path
        then putStrLn "No Solution"
        else putStrLn $ concatMap (\v -> show v ++ " -> ") path
    putStrLn ""

main :: IO ()
main = do
    putStrLn "Eulerian Path Finder"
    putStrLn "===================="
    mapM_ testCase testCases
