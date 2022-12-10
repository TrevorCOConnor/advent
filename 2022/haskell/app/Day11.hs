module Day11 (day11) where


fp :: FilePath
fp = "../data/Day11.txt"


day11 :: IO ()
day11 = do
    contents <- readFile fp
    let
        part1 = "todo"
        part2 = "todo"
    putStrLn "Day 11:"
    putStrLn $ "Day 1: " ++ show part1
    putStrLn $ "Day 2: " ++ show part2
