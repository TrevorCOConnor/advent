module Day13 (day13) where


fp :: FilePath
fp = "../data/Day13.txt"


day13 :: IO ()
day13 = do
    contents <- readFile fp
    let
        part1 = "todo"
        part2 = "todo"
    putStrLn "Day 13:"
    putStrLn $ "Day 1: " ++ show part1
    putStrLn $ "Day 2: " ++ show part2
