module Day15 (day15) where


fp :: FilePath
fp = "../data/Day15.txt"


day15 :: IO ()
day15 = do
    contents <- readFile fp
    let
        part1 = "todo"
        part2 = "todo"
    putStrLn "Day 15:"
    putStrLn $ "Day 1: " ++ show part1
    putStrLn $ "Day 2: " ++ show part2
