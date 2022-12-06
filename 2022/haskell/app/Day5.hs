module Day5 (day5) where

import qualified Data.Map as M
import Text.Regex.Posix
import Data.Char (isDigit, isSpace)


type CrateMap = M.Map Int String

fp :: FilePath
fp = "../data/Day5.txt"


takeEndReverse :: Int -> [a] -> [a]
takeEndReverse n ls = take n $ reverse ls


takeEnd :: Int -> [a] -> [a]
takeEnd n ls = reverse $ take n $ reverse ls


dropEnd :: Int -> [a] -> [a]
dropEnd n ls = reverse $ drop n $ reverse ls


transposeLines :: [String] -> [String]
transposeLines ([]:_) = []
transposeLines ls = map head ls : transposeLines (map tail ls)


exportCrates :: [String] -> M.Map Int String
exportCrates ls = foldl parseCrates M.empty crates
    where crates = transposeLines $ reverse $ takeWhile (not . null) ls
          parseCrates :: CrateMap -> String -> CrateMap
          parseCrates cm (c:cs) = if isDigit c
                                     then M.insert (read [c])
                                     (filter (not . isSpace) cs) cm
                                     else cm
          parseCrates cm _ = cm


moveCratesStack :: CrateMap -> [String] -> CrateMap
moveCratesStack = foldl applyMovement
    where applyMovement :: CrateMap -> String -> CrateMap
          applyMovement cm line = M.update (\x -> (x ++) <$> taken) to
                                  $ M.adjust (dropEnd amount) from cm
              where (amount, from, to) = extractMovementKeys line
                    taken = takeEndReverse amount <$> M.lookup from cm


moveCrates :: CrateMap -> [String] -> CrateMap
moveCrates = foldl applyMovement
    where applyMovement :: CrateMap -> String -> CrateMap
          applyMovement cm line = M.update (\x -> (x ++) <$> taken) to
                                  $ M.adjust (dropEnd amount) from cm
              where (amount, from, to) = extractMovementKeys line
                    taken = takeEnd amount <$> M.lookup from cm


extractMovementKeys :: String -> (Int, Int, Int)
extractMovementKeys line = (read f, read s, read t)
    where [f, s, t] = getAllTextMatches (line =~ "[0-9]+") :: [String]


day5 :: IO ()
day5 = do
    contents <- readFile fp
    let crateMap = exportCrates $ lines contents
    let movements = tail $ dropWhile (not . null) $ lines contents
    let part1 = map last $ M.elems $ moveCratesStack crateMap movements
    let part2 = map last $ M.elems $ moveCrates crateMap movements
    putStrLn "Day 5:"
    putStrLn $ "Part 1: " ++ show part1
    putStrLn $ "Part 2: " ++ show part2
