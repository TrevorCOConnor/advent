module Day8 (day8) where

import           Data.Map ((!))
import qualified Data.Map as M


type TreeMap = M.Map (Int, Int) Int
type Location = (Int, Int)
type Value = Int


data Direction = Above | Below | LeftOf | RightOf


fp :: FilePath
fp = "../data/Day8.txt"


findKeys :: Direction -> (Int, Int) -> Location -> [Location]
findKeys Above   _        (x, y) = [(x, n) | n <- [0..(y-1)]]
findKeys Below   (_, max) (x, y) = [(x, n) | n <- [(y+1)..max]]
findKeys LeftOf  _        (x, y) = [(n, y) | n <- [0..(x-1)]]
findKeys RightOf (max, _) (x, y) = [(n, y) | n <- [(x+1)..max]]


isVisible :: TreeMap -> (Int, Int) -> Location -> Value -> Bool
isVisible tm maxes loc = (> minimum [up, down, left, right])
    where up = maximum $ find Above
          down = maximum $ find Below
          left = maximum $ find LeftOf
          right = maximum $ find RightOf
          find direction = (-1) : map (\l -> M.findWithDefault (-1) l tm)
                                      (findKeys direction maxes loc)


visabilityScore :: TreeMap -> (Int, Int) -> Location -> Value -> Int
visabilityScore tm maxes loc value =
    product [topScore, bottomScore, leftScore, rightScore]
    where topScore = viewingDistance tm value $ reverse $ find Above
          bottomScore = viewingDistance tm value $ find Below
          leftScore = viewingDistance tm value $ reverse $ find LeftOf
          rightScore = viewingDistance tm value $ find RightOf
          find direction = findKeys direction maxes loc


viewingDistance :: TreeMap -> Value -> [Location] -> Int
viewingDistance tm value [] = 0
viewingDistance tm value (l:ls) =
    if value > M.findWithDefault (-1) l tm
       then 1 + viewingDistance tm value ls
       else 1


findMaxes :: TreeMap -> (Int, Int)
findMaxes tm = (maximum $ map fst $ M.keys tm, maximum $ map snd $ M.keys tm)


toTreeMap :: [String] -> TreeMap
toTreeMap ls = M.fromList $
    [ ((x, y), read [c])
      | (y, line) <- zip [0..] ls, (x, c) <- zip [0..] line ]


day8 :: IO ()
day8 = do
    contents <- readFile fp
    let
        treemap = toTreeMap $ lines contents
        maxes = findMaxes treemap
        part1 = length $ M.filterWithKey (isVisible treemap maxes) treemap
        part2 = maximum $ M.mapWithKey (visabilityScore treemap maxes) treemap
    putStrLn "Day 7:"
    putStrLn $ "Day 1: " ++ show part1
    putStrLn $ "Day 2: " ++ show part2
