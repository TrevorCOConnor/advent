module Day3 (day3) where

import System.IO
import Data.List

fp :: FilePath
fp = "../data/Day3.txt"

rucksackParse :: String -> Char
rucksackParse line = head $ leftSet `intersect` rightSet
    where size = length line `div` 2
          leftSet = take size line
          rightSet = drop size line


rucksackPriority :: Char -> Int
rucksackPriority c = case index of
                     Nothing -> error "Bad key"
                     Just i -> i + 1
    where index = elemIndex c $ ['a'..'z'] ++ ['A' .. 'Z']


rucksackGroupParse :: [String] -> Char
rucksackGroupParse (a:as) = head $ foldr intersect a as
rucksackGroupParse [] = error "List too small"


groupInto :: Int -> [a] -> [[a]]
groupInto n [] = []
groupInto n ls = take n ls : groupInto n (drop n ls)


day3 :: IO ()
day3 = do
    contents <- readFile fp
    let part1 = sum $ map (rucksackPriority . rucksackParse) $ lines contents
    let part2 = sum $ map (rucksackPriority . rucksackGroupParse) $ groupInto 3 $ lines contents
    putStrLn "Day 3:"
    putStrLn $ "Part 1: " ++ show part1
    putStrLn $ "Part 2: " ++ show part2
