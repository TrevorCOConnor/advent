module Day1 (day1) where

import System.IO
import Data.List
import Data.List.Split

fp :: FilePath
fp = "../data/Day1.txt"


day1 :: IO ()
day1 = do
    contents <- readFile fp
    let cals = splitOn [""] (lines contents)
    let sortedCals = sortOn negate $ map (sum . map read) cals :: [Int]
    putStrLn "Day 1:"
    putStrLn $ "Part 1: " ++ show (head sortedCals)
    putStrLn $ "Part 2: " ++ show (sum $ take 3 sortedCals)
