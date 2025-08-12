module Main where

{- | Check if a string is a palindrome recursively.
Time complexity: O(n)
Space complexity: O(n) due to recursion stack
-}
isPalindromeRecursive :: String -> Bool
isPalindromeRecursive str = isPalindromeHelper str 0 (length str - 1)
  where
    isPalindromeHelper :: String -> Int -> Int -> Bool
    isPalindromeHelper s left right
        | left >= right = True -- Base case: all characters checked
        | s !! left /= s !! right = False -- Characters don't match
        | otherwise = isPalindromeHelper s (left + 1) (right - 1) -- Move inward

{- | Alternative implementation using list pattern matching.
More idiomatic Haskell approach.
-}
isPalindrome :: String -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome (x : xs) = x == last xs && isPalindrome (init xs)

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
