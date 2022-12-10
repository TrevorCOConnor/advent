module Day24 (day24) where


fp :: FilePath
fp = "../data/Day24.txt"


day24 :: IO ()
day24 = do
    contents <- readFile fp
    let
        part1 = "todo"
        part2 = "todo"
    putStrLn "Day 24:"
    putStrLn $ "Day 1: " ++ show part1
    putStrLn $ "Day 2: " ++ show part2
