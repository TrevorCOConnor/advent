module Day22 (day22) where


fp :: FilePath
fp = "../data/Day22.txt"


day22 :: IO ()
day22 = do
    contents <- readFile fp
    let
        part1 = "todo"
        part2 = "todo"
    putStrLn "Day 22:"
    putStrLn $ "Day 1: " ++ show part1
    putStrLn $ "Day 2: " ++ show part2
