{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import System.Environment (getArgs)
import System.Process (callProcess)
import Control.Exception (try, IOException)
import Text.Printf
import Data.List (sortBy)
import Data.Ord (comparing)

-- | Algorithm registry with number, name, and executable
data Algorithm = Algorithm
    { algNumber :: Int
    , algName :: String
    , algExecutable :: String
    } deriving (Show, Eq)

-- | Complete list of implemented algorithms
algorithms :: [Algorithm]
algorithms = sortBy (comparing algNumber)
    [ Algorithm 1  "Eulerian Path/Hamiltonian Cycle" "algorithms-01-eulerian-path/algorithms-01-hamiltonian-cycle"
    , Algorithm 2  "Tree Detection" "algorithms-02-tree-detection"
    , Algorithm 3  "Subset Sum" "algorithms-03-subset-sum"
    , Algorithm 4  "Knapsack" "algorithms-04-knapsack"
    , Algorithm 5  "Linear Search" "algorithms-05-linear-search"
    , Algorithm 6  "Binary Search" "algorithms-06-binary-search"
    , Algorithm 7  "BFS Search" "algorithms-07-bfs-search"
    , Algorithm 8  "DFS Search" "algorithms-08-dfs-search"
    , Algorithm 9  "Bubble Sort" "algorithms-09-bubble-sort"
    , Algorithm 10 "Selection Sort" "algorithms-10-selection-sort"
    , Algorithm 11 "Insertion Sort" "algorithms-11-insertion-sort"
    , Algorithm 12 "Merge Sort" "algorithms-12-merge-sort"
    , Algorithm 13 "Quick Sort" "algorithms-13-quick-sort"
    , Algorithm 14 "Heap Sort" "algorithms-14-heap-sort"
    , Algorithm 17 "Recursion Palindrome" "algorithms-17-recursion-palindrome"
    , Algorithm 18 "Divide & Conquer Power" "algorithms-18-divide-and-conquer-power"
    ]

-- | Run a single algorithm
runAlgorithm :: Algorithm -> IO ()
runAlgorithm (Algorithm num name executable) = do
    printf "\n🔹 Running Algorithm %02d: %s\n" num name
    printf "════════════════════════════════════════════════════════\n"
    result <- try $ callProcess "stack" ["exec", executable]
    case result of
        Left (e :: IOException) -> printf "❌ Error running %s: %s\n" executable (show e)
        Right _ -> printf "✅ Algorithm %02d completed successfully\n" num

-- | Run all algorithms
runAllAlgorithms :: IO ()
runAllAlgorithms = do
    printf "\n🚀 Running All Haskell Algorithms (%d total)\n" (length algorithms)
    printf "════════════════════════════════════════════════════════\n"
    mapM_ runAlgorithm algorithms
    printf "\n🎉 All algorithms completed!\n"

-- | Find algorithms by number (can return multiple for special cases)
findAlgorithms :: Int -> [Algorithm]
findAlgorithms n
    | n == 1 = -- Special case: run both graph algorithms
        [ Algorithm 1 "Eulerian Path" "algorithms-01-eulerian-path"
        , Algorithm 1 "Hamiltonian Cycle" "algorithms-01-hamiltonian-cycle"
        ]
    | otherwise = filter (\alg -> algNumber alg == n) algorithms

-- | Show help message
showHelp :: IO ()
showHelp = do
    putStrLn "\nHaskell Algorithms Runner"
    putStrLn "========================="
    putStrLn "Usage:"
    putStrLn "  stack run                    - Show this help"
    putStrLn "  stack run all                - Run all algorithms"
    putStrLn "  stack run <number>           - Run specific algorithm by number"
    putStrLn ""
    putStrLn "Available algorithms:"
    mapM_ showAlgorithm algorithms
    putStrLn ""
  where
    showAlgorithm (Algorithm num name _) =
        printf "  %02d - %s\n" num name

-- | Parse command line arguments
parseArgs :: [String] -> IO ()
parseArgs [] = showHelp
parseArgs ["help"] = showHelp
parseArgs ["--help"] = showHelp
parseArgs ["-h"] = showHelp
parseArgs ["all"] = runAllAlgorithms
parseArgs [numberStr] =
    case reads numberStr of
        [(num, "")] ->
            case findAlgorithms num of
                [] -> do
                    printf "❌ Algorithm %d not found.\n" num
                    showHelp
                algs -> mapM_ runAlgorithm algs
        _ -> do
            putStrLn "❌ Invalid argument. Please provide a number or 'all'."
            showHelp
parseArgs args = do
    printf "❌ Too many arguments: %s\n" (unwords args)
    showHelp

main :: IO ()
main = do
    args <- getArgs
    parseArgs args
