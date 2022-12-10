module Day16 (day16) where


fp :: FilePath
fp = "../data/Day16.txt"


day16 :: IO ()
day16 = do
    contents <- readFile fp
    let
        part1 = "todo"
        part2 = "todo"
    putStrLn "Day 16:"
    putStrLn $ "Day 1: " ++ show part1
    putStrLn $ "Day 2: " ++ show part2
