module Day19 (day19) where


fp :: FilePath
fp = "../data/Day19.txt"


day19 :: IO ()
day19 = do
    contents <- readFile fp
    let
        part1 = "todo"
        part2 = "todo"
    putStrLn "Day 19:"
    putStrLn $ "Day 1: " ++ show part1
    putStrLn $ "Day 2: " ++ show part2
