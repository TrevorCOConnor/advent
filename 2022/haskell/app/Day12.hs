module Day12 (day12) where


fp :: FilePath
fp = "../data/Day12.txt"


day12 :: IO ()
day12 = do
    contents <- readFile fp
    let
        part1 = "todo"
        part2 = "todo"
    putStrLn "Day 12:"
    putStrLn $ "Day 1: " ++ show part1
    putStrLn $ "Day 2: " ++ show part2
