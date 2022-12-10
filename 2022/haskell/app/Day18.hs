module Day18 (day18) where


fp :: FilePath
fp = "../data/Day18.txt"


day18 :: IO ()
day18 = do
    contents <- readFile fp
    let
        part1 = "todo"
        part2 = "todo"
    putStrLn "Day 18:"
    putStrLn $ "Day 1: " ++ show part1
    putStrLn $ "Day 2: " ++ show part2
