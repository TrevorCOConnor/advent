module Day14 (day14) where


fp :: FilePath
fp = "../data/Day14.txt"


day14 :: IO ()
day14 = do
    contents <- readFile fp
    let
        part1 = "todo"
        part2 = "todo"
    putStrLn "Day 14:"
    putStrLn $ "Day 1: " ++ show part1
    putStrLn $ "Day 2: " ++ show part2
