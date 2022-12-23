{-# LANGUAGE NumericUnderscores #-}
module Day15 (day15) where

import Text.Regex.Posix
import qualified Data.Set as S
import Data.List

type Location = (Int, Int)
type Range = (Int, Int)
data Sensor = Sensor {center :: Location, radius :: Int}


fp :: FilePath
fp = "../data/Day15.txt"


parseSensorsAndBeacons :: String -> (Sensor, Location)
parseSensorsAndBeacons line = (Sensor center radius, beacon)
    where nums = map read $ getAllTextMatches (line =~ "[-0-9]+") :: [Int]
          center = (head nums, nums !! 1)
          beacon = (nums !! 2, nums !! 3)
          radius = dist center beacon


dist :: Location -> Location -> Int
dist (a, b) (c, d) = abs (c - a) + abs (d - b)


withinRadius :: Location -> Int -> Int -> (Int, Int)
withinRadius (a, b) r y = (min m p, max m p)
    where dy = abs (y - b)
          dx = abs (r - dy)
          m = a - dx 
          p = a + dx


withinSensor :: Sensor -> Int -> (Int, Int)
withinSensor sensor = withinRadius (center sensor) (radius sensor)


rangeIntersectionLeft :: Range -> Range -> Bool
rangeIntersectionLeft (a, b) (c, d) = c <= a && a <= d


mergeRangeLeft :: Range -> Range -> Range
mergeRangeLeft (a, b) (_, d) = (a, max b d)


rangeSort :: Range -> Range -> Ordering
rangeSort (a, b) (c, d) = if first == EQ then second else first
    where first = a `compare` c 
          second = b `compare` d


addRangeToChain :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
addRangeToChain [] range = [range]
addRangeToChain (r:rs) range = if rangeIntersectionLeft range r
                              then mergeRangeLeft r range : rs
                              else range : r : rs


chainLength :: [(Int, Int)] -> Int
chainLength = sum . map (\(a, b) -> b - a + 1)


day15 :: IO ()
day15 = do
    contents <- readFile fp
    let
        yValue = 2_000_000
        sensorsAndBeacons = map parseSensorsAndBeacons $ lines contents
        sensors = map fst sensorsAndBeacons
        beacons = nub $ map snd sensorsAndBeacons
        noBeaconsBySensors = chainLength $ foldl addRangeToChain []
                                         $ sortBy rangeSort
                                         $ map (`withinSensor` yValue) sensors
        inlineBeacons = map snd (filter ((== yValue) . snd) beacons)
        part1 = noBeaconsBySensors - length inlineBeacons
        part2 = "todo"
    putStrLn "Day 15:"
    putStrLn $ "Day 1: " ++ show part1
    putStrLn $ "Day 2: " ++ show part2
