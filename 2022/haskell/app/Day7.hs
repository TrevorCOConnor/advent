module Day7 (day7) where

import           Data.List
import           Data.List.Split
import           Text.Regex.Posix


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
            | "$ cd .." `isPrefixOf` l             = (ls, [])
            | "$ cd " `isPrefixOf` l = handleDir l ls
            | "dir " `isPrefixOf` l = buildDirContents ls
            | "$ ls" == l = buildDirContents ls
            | l =~ "[0-9]+ [A-Za-z.]+" = handleFile l ls
            | otherwise = error $ "unrecognized command" ++ show l

          handleDir :: String -> [String] -> ([String], [File])
          handleDir l ls = (rem', Directory name contents : contents')
            where (rem, contents) = buildDirContents ls
                  (rem', contents') = buildDirContents rem
                  (_, _, name) = l =~ "\\$ cd " :: (String, String, String)

          handleFile :: String -> [String] -> ([String], [File])
          handleFile l ls = (rem, File (last args) (read $ head args) : contents)
            where args = splitOn " " l
                  (rem, contents) = buildDirContents ls


fileSize :: File -> Int
fileSize (Directory name contents) = sum $ map fileSize contents
fileSize (File name s)             = s


allDirectories :: File -> [File]
allDirectories File {}                  = []
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
