module Day23 (day23) where


fp :: FilePath
fp = "../data/Day23.txt"


day23 :: IO ()
day23 = do
    contents <- readFile fp
    let
        part1 = "todo"
        part2 = "todo"
    putStrLn "Day 23:"
    putStrLn $ "Day 1: " ++ show part1
    putStrLn $ "Day 2: " ++ show part2
