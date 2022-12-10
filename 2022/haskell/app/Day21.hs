module Day21 (day21) where


fp :: FilePath
fp = "../data/Day21.txt"


day21 :: IO ()
day21 = do
    contents <- readFile fp
    let
        part1 = "todo"
        part2 = "todo"
    putStrLn "Day 21:"
    putStrLn $ "Day 1: " ++ show part1
    putStrLn $ "Day 2: " ++ show part2
