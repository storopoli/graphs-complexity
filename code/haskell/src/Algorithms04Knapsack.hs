module Main where

{- | Solve the 0/1 Knapsack problem using brute force.
Time complexity: O(2^n)
Space complexity: O(n) due to recursion depth
-}
knapsack ::
    -- | Remaining capacity of the knapsack
    Int ->
    -- | Weights of the items
    [Int] ->
    -- | Values of the items
    [Int] ->
    -- | Number of items remaining to consider
    Int ->
    -- | Maximum value that can be obtained with the given capacity
    Int
knapsack _ _ _ 0 = 0
knapsack 0 _ _ _ = 0
knapsack w weights values n
    | weights !! (n - 1) > w = knapsack w weights values (n - 1)
    | otherwise = max includeItem excludeItem
  where
    includeItem = values !! (n - 1) + knapsack (w - weights !! (n - 1)) weights values (n - 1)
    excludeItem = knapsack w weights values (n - 1)

main :: IO ()
main = do
    let values = [60, 100, 120] -- Values of the items
    let weights = [10, 20, 30] -- Weights of the items
    let w = 50 -- Maximum capacity of the knapsack
    let n = length values -- Number of items
    let maxValue = knapsack w weights values n

    putStrLn $ "Maximum value that can be carried: " ++ show maxValue
