module Day4 (day4) where

import Data.List.Split

fp :: FilePath
fp = "../data/Day4.txt"

data Range = Range {lower :: Int , upper :: Int}

completeOverlap :: Range -> Range -> Bool
completeOverlap left right = compare left right || compare right left
    where compare x y  = (lower x <= lower y) && (upper x >= upper y)


overlap :: Range -> Range -> Bool
overlap left right = compare left right || compare right left
    where compare x y = lower y <= lower x && lower x <= upper y


toRange :: String -> Range
toRange text = Range (read x) (read y)
    where (x: y: extra) = splitOn "-" text


toRangePair :: String -> (Range, Range)
toRangePair text = (toRange left, toRange right)
    where (left: right: extra) = splitOn "," text


day4 :: IO ()
day4 = do
    contents <- readFile fp
    let pairs = map toRangePair $ lines contents
    let part1 = length $ filter (uncurry completeOverlap) pairs
    let part2 = length $ filter (uncurry overlap) pairs
    putStrLn "Day 4:"
    putStrLn $ "Part 1: " ++ show part1
    putStrLn $ "Part 2: " ++ show part2
