module Day20 (day20) where


fp :: FilePath
fp = "../data/Day20.txt"


day20 :: IO ()
day20 = do
    contents <- readFile fp
    let
        part1 = "todo"
        part2 = "todo"
    putStrLn "Day 20:"
    putStrLn $ "Day 1: " ++ show part1
    putStrLn $ "Day 2: " ++ show part2
