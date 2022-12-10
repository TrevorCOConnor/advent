module Day9 (day9) where

import Data.List.Split
import Data.List (nub)


type Location = (Int, Int)


fp :: FilePath
fp = "../data/Day9.txt"


average :: Int -> Int -> Int
average x y = if even diff
                 then diff `div` 2 + x
                 else (diff `rem` 2) + x
    where diff = y - x


distance :: Location -> Location -> Int
distance (a, b) (x, y) = max (abs (x - a)) (abs (y - b))


mapByDirection :: String -> Location -> Location
mapByDirection "U" (x, y) = (x, y + 1)
mapByDirection "D" (x, y) = (x, y - 1)
mapByDirection "L" (x, y) = (x - 1, y)
mapByDirection "R" (x, y) = (x + 1, y)
mapByDirection s _ = error $ "Unrecognized key" ++ s


applyMovement :: String -> Int -> Location -> [Location]
applyMovement s n = take n . tail . iterate (mapByDirection s)


trackHead :: Location -> [String] -> [Location]
trackHead ropeHead = foldl f [ropeHead]
    where f hs line = let args = splitOn " " line :: [String]
                      in hs ++ applyMovement (head args) (read $ last args) (last hs)


trackTail :: Location -> [Location] -> [Location]
trackTail ropeTail = tail . reverse . foldl (\ts h -> moveTail (head ts) h : ts) [ropeTail]


moveTail :: Location -> Location -> Location
moveTail tail head =
    if distance tail head > 1
       then mv tail head
       else tail
    where mv (a, b) (x, y) = (average a x, average b y)


day9 :: IO ()
day9 = do
    contents <- readFile fp
    let
        headMovement = trackHead (0, 0) $ lines contents
        part1 = length $ nub $ trackTail (0, 0) headMovement
        part2 = length $ nub $ last $ take 9 $ tail $ iterate (trackTail (0, 0)) headMovement
    putStrLn "Day 9:"
    putStrLn $ "Day 1: " ++ show part1
    putStrLn $ "Day 2: " ++ show part2
