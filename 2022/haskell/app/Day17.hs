module Day17 (day17) where


fp :: FilePath
fp = "../data/Day17.txt"


day17 :: IO ()
day17 = do
    contents <- readFile fp
    let
        part1 = "todo"
        part2 = "todo"
    putStrLn "Day 17:"
    putStrLn $ "Day 1: " ++ show part1
    putStrLn $ "Day 2: " ++ show part2
