module Main where

import Text.Printf
import Utils

{- | Calculate x raised to the power n using divide and conquer.
Time complexity: O(log n)
Space complexity: O(log n) due to recursion stack
-}
power :: Double -> Int -> Double
power x n = powerInteger x (toInteger n)
  where
    powerInteger _ 0 = 1.0
    powerInteger base expN
        | expN < 0 = 1.0 / powerInteger base (-expN)
        | even expN = let half = powerInteger base (expN `div` 2) in half * half
        | otherwise = base * powerInteger base (expN - 1)

{- | Naive iterative power calculation for comparison.
Time complexity: O(n)
Space complexity: O(1)
-}
powerNaive :: Double -> Int -> Double
powerNaive x n = powerNaiveInteger x (toInteger n)
  where
    powerNaiveInteger _ 0 = 1.0
    powerNaiveInteger base expN
        | expN < 0 = 1.0 / powerNaiveInteger base (-expN)
        | otherwise = base * powerNaiveInteger base (expN - 1)

-- | Test power calculation with timing
testPower :: Double -> Int -> IO ()
testPower x n = do
    printf "Calculating %.1f^%d\n" x n

    -- Test divide and conquer version
    (result1, time1) <- timeAction $ return (power x n)
    printf "Divide & Conquer - Result: %e, Time: %f seconds\n" result1 time1

    -- Test naive version (but only for smaller values to avoid stack overflow)
    if n <= 100
        then do
            (result2, time2) <- timeAction $ return (powerNaive x n)
            printf "Naive Recursion - Result: %e, Time: %f seconds\n" result2 time2
        else putStrLn "Naive Recursion - Skipped (too large)"

    putStrLn ""

main :: IO ()
main = do
    putStrLn "Power Calculation - Divide and Conquer vs Naive"
    putStrLn "==============================================="
    let x = 2.0
    let nValues = [10, 20, 40, 80, 160, 320, 640]
    mapM_ (testPower x) nValues
