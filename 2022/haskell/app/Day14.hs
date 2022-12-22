module Day14 (day14) where

import Data.List.Split
import qualified Data.Map as M

fp :: FilePath
fp = "../data/Day14.txt"

type Location = (Int, Int)

type FallenMap = M.Map Int [Int]

data Fallen = Landed FallenMap
            | Terminated FallenMap


startPoint :: Location
startPoint = (500, 0)


createLine :: String -> [Location]
createLine line = chainSegments points
    where points = map (\x -> read ("(" ++ x ++ ")")) $ splitOn " -> " line :: [Location]


chainSegments :: [Location] -> [Location]
chainSegments [a,b] = createSegment a b
chainSegments (a:b:cs) = createSegment a b ++ chainSegments (b:cs)
chainSegments _ = error "Ahhh!"


createSegment :: Location -> Location -> [Location]
createSegment (a, b) (x, y) =
    if a /= x
       then [(k, b) | k <- [(min a x)..(max a x)]]
       else [(a, k) | k <- [(min b y)..(max b y)]]


dropLoc :: Location -> Location
dropLoc (a, b) = (a, b+1)


leftLoc :: Location -> Location
leftLoc (a, b) = (a-1, b)


rightLoc :: Location -> Location
rightLoc (a, b) = (a+1, b)


dropLocations :: Location -> (Location, Location, Location)
dropLocations loc = (dropLoc loc, dropLoc . leftLoc $ loc, dropLoc . rightLoc $ loc)


checkIfFallen :: Location -> FallenMap -> Bool
checkIfFallen (a, b) fallen = b `elem` M.findWithDefault [] a fallen


markFallen :: Location -> FallenMap -> FallenMap
markFallen (a, b) = M.insertWith (++) a [b]


fallAbyss :: Int -> Location -> FallenMap -> Fallen
fallAbyss max loc fallen
    | snd loc >= max = Terminated fallen
    | not $ checkIfFallen dropped fallen = fallAbyss max dropped fallen
    | not $ checkIfFallen lefted fallen = fallAbyss max lefted fallen
    | not $ checkIfFallen righted fallen = fallAbyss max righted fallen
    | otherwise = Landed $ markFallen loc fallen
    where (dropped, lefted, righted) = dropLocations loc


fallFloor :: Int -> Location -> Location -> FallenMap -> Fallen
fallFloor max start loc fallen
    | loc == start && checkIfFallen start fallen = Terminated fallen
    | snd dropped == max + 2 = Landed $ markFallen loc fallen
    | not $ checkIfFallen dropped fallen = fallFloor max start dropped fallen
    | not $ checkIfFallen lefted fallen = fallFloor max start lefted fallen
    | not $ checkIfFallen righted fallen = fallFloor max start righted fallen
    | otherwise = Landed $ markFallen loc fallen
    where (dropped, lefted, righted) = dropLocations loc


buildCollection :: (FallenMap -> Fallen) -> FallenMap -> FallenMap
buildCollection fallFunc collection =
    case fell of
      Landed new -> do
          buildCollection fallFunc new
      Terminated new -> new
    where fell = fallFunc collection


day14 :: IO ()
day14 = do
    contents <- readFile fp
    let
        walls = concatMap createLine $ lines contents
        fallenMap = foldl (flip markFallen) M.empty walls
        fallenMax = maximum $ concat $ M.elems fallenMap
        first = buildCollection (fallAbyss fallenMax startPoint) fallenMap
        part1 = length (concat $ M.elems first) - length walls
        second = buildCollection (fallFloor fallenMax startPoint startPoint) first
        part2 = length (concat $ M.elems second) - length walls
    putStrLn "Day 14:"
    putStrLn $ "Day 1: " ++ show part1
    putStrLn $ "Day 2: " ++ show part2
