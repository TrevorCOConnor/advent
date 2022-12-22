module Day14 (day14) where

import Data.List.Split

fp :: FilePath
fp = "../data/Day14.txt"

type Location = (Int, Int)

data Collection = Collection { fallen :: [Location], walls :: [Location] }


data Fallen = Landed Collection
            | Terminated Collection


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


fallAbyss :: Int -> Location -> Collection -> Fallen
fallAbyss max loc collection
    | snd loc >= max = Terminated collection
    | dropped `notElem` fallen collection ++ walls collection =
        fallAbyss max dropped collection
    | lefted `notElem` fallen collection ++ walls collection =
        fallAbyss max lefted collection
    | righted `notElem` fallen collection ++ walls collection =
        fallAbyss max righted collection
    | otherwise = Landed $ collection {fallen=loc : fallen collection}
    where (dropped, lefted, righted) = dropLocations loc


fallFloor :: Int -> Location -> Location -> Collection -> Fallen
fallFloor max start loc collection
    | loc == start && start `elem` fallen collection = Terminated collection
    | snd dropped == max + 2 = Landed $ collection { fallen=loc : fallen collection}
    | dropped `notElem` fallen collection ++ walls collection =
        fallFloor max start dropped collection
    | lefted `notElem` fallen collection ++ walls collection =
        fallFloor max start lefted collection
    | righted `notElem` fallen collection ++ walls collection =
        fallFloor max start righted collection
    | otherwise = Landed $ collection {fallen=loc : fallen collection}
    where (dropped, lefted, righted) = dropLocations loc


countSandIO :: (Collection -> Fallen) -> Int -> Collection -> IO Int
countSandIO fallFunc num collection =
    case fell of
      Landed new -> do
          print num
          countSandIO fallFunc (num + 1) new
      Terminated new -> return num
    where fell = fallFunc collection


buildCollection :: (Collection -> Fallen) -> Collection -> Collection
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
        collection = Collection [] $ concatMap createLine $ lines contents
        collectionMax = maximum $ map snd $ walls collection
        -- part1 = countSand (fallAbyss collectionMax startPoint) collection
        -- part2 = countSand (fallFloor collectionMax startPoint startPoint) collection
        first = buildCollection (fallAbyss collectionMax startPoint) collection
        part1 = length $ fallen first
        -- second = buildCollection (fallFloor collectionMax startPoint startPoint) first
        -- part2 = length $ fallen second
    countSandIO (fallFloor collectionMax startPoint startPoint) 0 collection
    putStrLn "Day 14:"
    -- putStrLn $ "Day 1: " ++ show part1
    -- putStrLn $ "Day 2: " ++ show part2
