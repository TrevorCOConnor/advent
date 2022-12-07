module Day7 (day7) where

import Data.List
import Data.List.Split
import Text.Regex.Posix


fp :: FilePath
fp = "../data/Day7.txt"

dirMazSize :: Int
dirMazSize = 100000


totalSpace :: Int
totalSpace = 70000000


neededSpace :: Int
neededSpace = 30000000


data File = Directory {dirName :: String, contents :: [File]}
          | File {fileName :: String, size :: Int}
          deriving (Show)


buildDir :: [String] -> File
buildDir ls = head contents
    where ([], contents) = buildDirContents ls
          buildDirContents :: [String] -> ([String], [File])
          buildDirContents [] = ([], [])
          buildDirContents (l:ls)
            | "$ cd .." `isPrefixOf` l = (ls, [])
            | "$ cd " `isPrefixOf` l = let (rem', contents') = buildDirContents rem
                                           (_, _, name) = l =~ "\\$ cd " :: (String, String, String)
                                       in (rem', Directory name contents : contents')
            | "dir " `isPrefixOf` l || "$ ls" == l = buildDirContents ls
            | l =~ "[0-9]+ [A-Za-z.]+" = let args = splitOn " " l in (rem, File (last args) (read $ head args) : contents)
            | otherwise = error $ "unrecognized command" ++ show l
              where (rem, contents) = buildDirContents ls


fileSize :: File -> Int
fileSize (Directory name contents) = sum $ map fileSize contents
fileSize (File name s) = s


allDirectories :: File -> [File]
allDirectories File {} = []
allDirectories d@(Directory _ contents) = d : concatMap allDirectories contents


day7 :: IO ()
day7 = do
    contents <- readFile fp
    let dir = buildDir $ lines contents
        dirs = allDirectories dir
        fileSizes = map fileSize dirs
        spaceToFree = neededSpace - (totalSpace - fileSize dir)
        part1 = sum $ filter (< dirMazSize) fileSizes
        part2 = minimum $ filter (>= spaceToFree) fileSizes
    putStrLn "Day 7:"
    putStrLn $ "Day 1: " ++ show part1
    putStrLn $ "Day 2: " ++ show part2
