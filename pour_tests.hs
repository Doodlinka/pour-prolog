import Pour

-- Helper to print the result nicely
printResult :: String -> Maybe Path -> IO ()
printResult name Nothing = putStrLn $ name ++ ": No solution"
printResult name (Just path) = do
    putStrLn $ "=== " ++ name ++ " ==="
    putStrLn $ "Steps: " ++ show (length path - 1)
    mapM_ print path -- Print each state on a new line
    putStrLn ""

main :: IO ()
main = do
    putStrLn "Running Haskell Tests...\n"

    -- TEST 1: Die Hard (5L, 3L -> 4L)
    -- Start: [0, 0] (buckets 0 and 1)
    -- Caps:  [5, 3]
    let caps1 = [5, 3]
    let start1 = [0, 0]
    printResult "Test 1 (Die Hard Open)" (solveNumOpen caps1 start1 4)

    -- TEST 2: Closed System (8L, 5L, 3L -> 4L)
    -- Start: [8, 0, 0]
    -- Caps:  [8, 5, 3]
    let caps2 = [8, 5, 3]
    let start2 = [8, 0, 0]
    printResult "Test 2 (Closed 8-5-3)" (solveNumClosed caps2 start2 4)

    -- TEST 3: Impossible (2L, 6L -> 3L)
    -- Should return Nothing immediately (if BFS) or very strictly (if DFS w/ visited)
    let caps3 = [2, 6]
    let start3 = [0, 0]
    printResult "Test 3 (Impossible)" (solveNumOpen caps3 start3 3)

    -- TEST 4: Exact State
    -- From [0, 5, 0] to [5, 0, 0] in a closed system
    let caps4 = [5, 5, 5]
    let start4 = [0, 5, 0]
    let target4 = [5, 0, 0]
    printResult "Test 4 (Target State)" (solveClosed caps4 start4 target4)