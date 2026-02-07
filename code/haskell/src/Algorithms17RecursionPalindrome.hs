module Main where

import Data.Array (Array, listArray, (!))

{- | Check if a string is a palindrome recursively.
Time complexity: O(n)
Space complexity: O(n) due to recursion stack
-}
isPalindromeRecursive :: String -> Bool
isPalindromeRecursive str
    | n <= 1 = True
    | otherwise = isPalindromeHelper 0 (n - 1)
  where
    n = length str
    arr = listArray (0, n - 1) str :: Array Int Char

    isPalindromeHelper :: Int -> Int -> Bool
    isPalindromeHelper left right
        | left >= right = True -- Base case: all characters checked
        | arr ! left /= arr ! right = False -- Characters don't match
        | otherwise = isPalindromeHelper (left + 1) (right - 1) -- Move inward

{- | Alternative implementation using reverse.
Simple and linear-time for lists.
-}
isPalindrome :: String -> Bool
isPalindrome s = s == reverse s

-- | Test strings for palindrome checking
testStrings :: [String]
testStrings =
    [ "radar"
    , "level"
    , "hello"
    , "racecar"
    , "madam"
    , "steponnopets" -- "step on no pets" without spaces
    , ""
    , "abcba"
    , "notapalindrome"
    ]

-- | Test a single string with both implementations
testString :: String -> IO ()
testString str = do
    putStrLn $ "Testing: \"" ++ str ++ "\""
    putStrLn $ "Recursive result: " ++ if isPalindromeRecursive str then "It's a palindrome." else "It's not a palindrome."
    putStrLn $ "Pattern match result: " ++ if isPalindrome str then "It's a palindrome." else "It's not a palindrome."
    putStrLn ""

main :: IO ()
main = do
    putStrLn "Palindrome Checker - Recursion vs Pattern Matching"
    putStrLn "=================================================="
    mapM_ testString testStrings
