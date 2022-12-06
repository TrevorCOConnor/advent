module Day6 (day6) where

import Data.List

fp :: FilePath
fp = "../data/Day6.txt"


startOfPacketLength :: Int
startOfPacketLength = 4


startOfMessageLength :: Int
startOfMessageLength = 14


isStartOfPacket :: Int -> String -> Bool
isStartOfPacket len ls = length (nub $ take len ls) == len


findStartIndex :: Int -> String -> Int
findStartIndex len [] = error "Never found start"
findStartIndex len line@(c:cs) =
    if isStartOfPacket len line
       then len
       else 1 + findStartIndex len cs


day6 :: IO ()
day6 = do
    contents <- readFile fp
    let part1 = findStartIndex startOfPacketLength contents
    let part2 = findStartIndex startOfMessageLength contents
    putStrLn "Day 6:"
    putStrLn $ "Day 1: " ++ show part1
    putStrLn $ "Day 2: " ++ show part2
