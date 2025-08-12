module Main where

import Data.Array

-- | Graph represented as adjacency matrix
type AdjMatrix = Array (Int, Int) Int

-- | Create adjacency matrix from list of lists
createAdjMatrix :: [[Int]] -> AdjMatrix
createAdjMatrix matrix =
    let n = length matrix
    in array ((0, 0), (n-1, n-1))
             [((i, j), (matrix !! i) !! j) | i <- [0..n-1], j <- [0..n-1]]

-- | Get number of vertices in the graph
graphSize :: AdjMatrix -> Int
graphSize arr = let ((_, _), (n, _)) = bounds arr in n + 1

{- | Find Hamiltonian cycle using backtracking.
Time complexity: O(N!)
Space complexity: O(N)
-}
findHamiltonianCycle :: AdjMatrix -> Maybe [Int]
findHamiltonianCycle graph =
    let n = graphSize graph
        path = replicate n (-1)
    in hamiltonianUtil graph (0 : tail path) 1 n

-- | Utility function for backtracking
hamiltonianUtil :: AdjMatrix -> [Int] -> Int -> Int -> Maybe [Int]
hamiltonianUtil graph path pos n
    | pos == n =
        -- Check if last vertex connects to first vertex
        if graph ! (path !! (n-1), head path) == 1
        then Just path
        else Nothing
    | otherwise =
        -- Try each vertex as next candidate
        tryVertices [1..n-1] path pos
  where
    tryVertices [] _ _ = Nothing
    tryVertices (v:vs) currentPath currentPos
        | isSafe v currentPath currentPos =
            case hamiltonianUtil graph newPath (currentPos + 1) n of
                Just result -> Just result
                Nothing -> tryVertices vs currentPath currentPos
        | otherwise = tryVertices vs currentPath currentPos
      where
        newPath = take currentPos currentPath ++ [v] ++ drop (currentPos + 1) currentPath

        isSafe vertex pathSoFar position =
            -- Check if vertex is adjacent to previous vertex
            graph ! (pathSoFar !! (position - 1), vertex) == 1 &&
            -- Check if vertex is not already in path
            vertex `notElem` take position pathSoFar

-- | Test cases from C implementation
testCases :: [([[Int]], String)]
testCases =
    [ ( [[0,1,0,1,0],
         [1,0,1,1,1],
         [0,1,0,0,1],
         [1,1,0,0,1],
         [0,1,1,1,0]]
      , "Test Case 1" )
    , ( [[0,1,0,1,0],
         [1,0,1,1,1],
         [0,1,0,0,1],
         [1,1,0,0,0],
         [0,1,1,0,0]]
      , "Test Case 2" )
    ]

-- | Test a single case
testCase :: ([[Int]], String) -> IO ()
testCase (matrix, name) = do
    putStrLn $ name ++ ":"
    let graph = createAdjMatrix matrix
    case findHamiltonianCycle graph of
        Nothing -> putStrLn "Solution does not exist"
        Just cycle -> do
            putStr "Solution Exists: "
            mapM_ (\v -> putStr $ show v ++ " ") cycle
            putStr $ show (head cycle) ++ " "  -- Complete the cycle
            putStrLn ""
    putStrLn ""

main :: IO ()
main = do
    putStrLn "Hamiltonian Cycle Finder"
    putStrLn "========================"
    mapM_ testCase testCases
