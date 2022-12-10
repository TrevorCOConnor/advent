module Day25 (day25) where


fp :: FilePath
fp = "../data/Day25.txt"


day25 :: IO ()
day25 = do
    contents <- readFile fp
    let
        part1 = "todo"
        part2 = "todo"
    putStrLn "Day 25:"
    putStrLn $ "Day 1: " ++ show part1
    putStrLn $ "Day 2: " ++ show part2
