{-# LANGUAGE NumericUnderscores #-}
module Day15 (day15) where

import Text.Regex.Posix
import qualified Data.Set as S
import Data.List
import Data.Maybe

type Location = (Int, Int)
type Range = (Int, Int)
data Sensor = Sensor {center :: Location, radius :: Int}
    deriving (Show)


part1Column :: Int
-- part1Column = 10
part1Column = 2_000_000

part2Max :: Int
part2Max = 4_000_000
-- part2Max = 20


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


rangeWithinRadiusOfY :: Location -> Int -> Int -> Maybe Range
rangeWithinRadiusOfY (a, b) r y = if dx > 0
                                     then Just (min m p, max m p)
                                     else Nothing
    where dy = abs (y - b)
          dx = r - dy
          m = a - dx 
          p = a + dx


rangeWithinSensorOfY :: Sensor -> Int -> Maybe Range
rangeWithinSensorOfY sensor = rangeWithinRadiusOfY (center sensor) (radius sensor)


withinSensor :: Location -> Sensor -> Bool
withinSensor loc (Sensor cent r) = dist cent loc <= r


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


withinRange :: Int -> Range -> Bool
withinRange x (a, b) = a <= x && x <= b


rangeDifference :: Range -> Range -> Int
rangeDifference (a, b) (c, d) = min b d + 1


checkRow :: [Sensor] -> Int -> [Range]
checkRow sensors column =
    foldl addRangeToChain [] $
        sortBy rangeSort $
            mapMaybe (`rangeWithinSensorOfY` column) sensors


tuning :: Location -> Int
tuning (x, y) = x * 4_000_000 + y


day15 :: IO ()
day15 = do
    contents <- readFile fp
    let
        sensorsAndBeacons = map parseSensorsAndBeacons $ lines contents
        sensors = map fst sensorsAndBeacons
        beacons = nub $ map snd sensorsAndBeacons
        noBeaconsBySensors = chainLength $ sensors `checkRow` part1Column
        inlineBeacons = map snd (filter ((== part1Column) . snd) beacons)
        part1 = noBeaconsBySensors - length inlineBeacons

    let 
        (range, y) = head $ filter ((> 1) . length . fst) $
            (`zip` [0..part2Max]) $
                map (sensors `checkRow`) [0..part2Max]
        x = rangeDifference (head range) (range !! 1)
        part2 = tuning (x, y)
    putStrLn "Day 15:"
    putStrLn $ "Day 1: " ++ show part1
    putStrLn $ "Day 2: " ++ show part2
