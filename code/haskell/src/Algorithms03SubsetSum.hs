module Main where

import Data.Bits (shiftL, (.&.))

{- | Verifies if there is a subset that sums to the target value.
Uses brute force approach by checking all possible subsets (2^n combinations).
Time complexity: O(n * 2^n)
Space complexity: O(1)
-}
subsetSum ::
    -- | The set of numbers
    [Int] ->
    -- | The target sum
    Int ->
    -- | True if there is a subset that sums to the target, False otherwise
    Bool
subsetSum set target = any hasTarget [0 .. numSubsets - 1]
  where
    n = length set
    numSubsets = shiftL 1 n
    hasTarget i = sum (filterByBitMask i set) == target

    -- \| Filters elements from the list based on the bit mask
    filterByBitMask :: Int -> [Int] -> [Int]
    filterByBitMask mask xs = [x | (j, x) <- zip [0 ..] xs, mask .&. shiftL 1 j /= 0]

main :: IO ()
main = do
    let set = [1, 2, 3, 4, 5]
    let target = 9

    if subsetSum set target
        then putStrLn $ "Yes, there is a subset that sums to " ++ show target
        else putStrLn $ "No, there isn't a subset that sums to " ++ show target
