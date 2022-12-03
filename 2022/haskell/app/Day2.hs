module Day2 (day2) where

import           System.IO

fp :: FilePath
fp = "../data/Day2.txt"

data Move = Rock | Paper | Scissors
    deriving (Eq)


-- Constants
win = 6 :: Int
tie = 3 :: Int
loss = 0 :: Int


-- Part 2 functions
toDraw :: Move -> Move
toDraw = id  -- toWin . toWin . toWin

toWin :: Move -> Move
toWin Rock = Paper
toWin Scissors = Rock
toWin Paper = Scissors

toLose :: Move -> Move
toLose = toWin . toWin


-- Part 1 functions
compareMoves :: Move -> Move -> Int
compareMoves left right 
  | left == right = tie
  | left == toWin right = win
  | otherwise = loss


decodeToMove :: String -> Move
decodeToMove "X" = Rock
decodeToMove "Y" = Paper
decodeToMove "Z" = Scissors
decodeToMove "A" = Rock
decodeToMove "B" = Paper
decodeToMove "C" = Scissors
decodeToMove s   = error $ "Broken key: " ++ s


decodeToFunction :: String -> (Move -> Move)
decodeToFunction "X" = toLose
decodeToFunction "Y" = toDraw
decodeToFunction "Z" = toWin
decodeToFunction s = error $ "Broken key" ++ s


weight :: Move -> Int
weight Rock     = 1
weight Paper    = 2
weight Scissors = 3


playRound1 :: String -> String -> Int
playRound1 theirs ours = weight ourMove + compareMoves ourMove theirMove
    where ourMove = decodeToMove ours
          theirMove = decodeToMove theirs


playRound2 :: String -> String -> Int
playRound2 theirs plan = weight ourMove + compareMoves ourMove theirMove
    where ourPlan = decodeToFunction plan
          theirMove = decodeToMove theirs
          ourMove = ourPlan theirMove


toRound1 :: [String] -> Int
toRound1 ws = if length ws >= 2
                then playRound1 (head ws) (ws !! 1)
                else error "Broken round"


toRound2 :: [String] -> Int
toRound2 ws = if length ws >= 2
                then playRound2 (head ws) (ws !! 1)
                else error "Broken round"


day2 :: IO ()
day2 = do
    contents <- readFile fp
    let part1 = sum $ map (toRound1 . words) $ lines contents
    let part2 = sum $ map (toRound2 . words) $ lines contents
    putStrLn $ "Part 1: " ++ show part1
    putStrLn $ "Part 2: " ++ show part2
