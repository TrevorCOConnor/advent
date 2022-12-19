module Day12 (day12) where

import Data.Char (ord)
import qualified Data.Map as M
import Data.List (sortOn)
import Control.Concurrent (threadDelay)
import Control.Monad (forM_)
import Data.Maybe


type Location = (Int, Int)
type FieldMap = M.Map Location Char
type Maxes = (Int, Int)


fp :: FilePath
fp = "../data/Day12.txt"


data PathTree = Path Location PathTree
              | Branch [PathTree]
              | Fail
              | End
              deriving (Show)

type WalkedPath = (PathTree, [Location])


isEnd :: PathTree -> Bool
isEnd End = True
isEnd _ = False


-- Take one step down each path
steps :: [Location] -> [PathTree] -> Int
steps history pt = if null ended
                  then 1 + steps nextHistory nextPaths
                  else 1
    where ended = filter isEnd nextPaths
          (nextHistory, nextPaths) = _steps [] history pt


maybeSteps :: [Location] -> [PathTree] -> Maybe Int
maybeSteps history pt
  | null nextPaths = Nothing
  | null ended = (1+) <$> maybeSteps nextHistory nextPaths
  | otherwise = Just 1
  where ended = filter isEnd nextPaths
        (nextHistory, nextPaths) = _steps [] history pt


_steps :: [PathTree] -> [Location] -> [PathTree] -> ([Location], [PathTree])
_steps done history [] = (history, done)
_steps done history (pt: pts) =
    case pt of
      (Path loc pt') -> if loc `elem` history
                          then _steps done history pts
                          else _steps (pt':done) (loc:history) pts
      (Branch pts') -> _steps done history (pts' ++ pts)
      Fail -> _steps done history pts
      End -> ([], [])


comparePoint :: FieldMap -> Int -> [Location] -> Int
comparePoint fm currentMax [] = currentMax
comparePoint fm currentMax (loc:locs) = 
    if isJust next && fromJust next < currentMax
       then comparePoint fm (fromJust next) locs
       else comparePoint fm currentMax locs
   where next = maybeSteps [] [explorePaths loc fm]


createFieldMap :: [String] -> FieldMap
createFieldMap lines = M.fromList
    [ ((x, y), char) | (y, line) <- numberedLines, (x, char) <- zip [0..] line ]
    where numberedLines = zip [0..] lines


findStart :: FieldMap -> Location
findStart = head . M.keys . M.filter (== 'S')


isTraversable :: Char -> Char -> Bool
isTraversable 'S' next = next == 'a'
isTraversable current 'E' = current == 'z'
isTraversable current next = ord next - ord current <= 1


getPossibleLocations :: Location -> Location -> [Location]
getPossibleLocations (mx, my) (x, y) = filter
    (\(a, b) -> a >= 0 && b >= 0 && a <= mx && b <= my)
    [ (x-1, y), (x+1, y), (x, y-1), (x, y+1) ]


getTraversableLocations :: Maxes -> Location -> [Location] -> FieldMap -> [Location]
getTraversableLocations maxes current past fm =
    filter (isTraversable (fm M.! current) . (fm M.!)) $
        filter (not . (`elem` past)) $
            getPossibleLocations maxes current


explorePaths :: Location -> FieldMap -> PathTree
explorePaths loc fm = _explorePaths (mx, my) [] loc fm
    where mx = maximum $ map fst $ M.keys fm
          my = maximum $ map snd $ M.keys fm


_explorePaths :: Maxes -> [Location] -> Location -> FieldMap -> PathTree
_explorePaths maxes history loc fm
    | not (null ended) = Path (head ended) End
    | size == 0 = Fail
    | size == 1 = Path (head nexts) $ _explorePaths maxes (loc:history) (head nexts) fm
    | otherwise = Branch $ map (\l -> Path l $ _explorePaths maxes (loc:history) l fm) nexts
    where nexts = getTraversableLocations maxes loc history fm
          ended = filter ((== 'E') . (fm M.!)) nexts
          size = length nexts


day12 :: IO ()
day12 = do
    contents <- readFile fp
    let
        fieldMap = createFieldMap $ lines contents
        start = findStart fieldMap
        pathTree = explorePaths start fieldMap
        possibleStarts = M.keys $ M.filter (=='a') fieldMap
        part1 = steps [] [pathTree]
        part2 = comparePoint fieldMap part1 possibleStarts
    putStrLn "Day 12:"
    putStrLn $ "Day 1: " ++ show part1
    putStrLn $ "Day 2: " ++ show part2
