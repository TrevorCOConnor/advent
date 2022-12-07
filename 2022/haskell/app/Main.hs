module Main where

-- Standard
import System.Environment
import Control.Monad
import Data.Char (toLower)

-- Local
import Day1
import Day2
import Day3
import Day4
import Day5
import Day6
import Day7

main :: IO ()
main = do
    args' <- getArgs
    let args = map (map toLower) args'
    when ("day1" `elem` args || null args) day1
    when ("day2" `elem` args || null args) day2
    when ("day3" `elem` args || null args) day3
    when ("day4" `elem` args || null args) day4
    when ("day5" `elem` args || null args) day5
    when ("day6" `elem` args || null args) day6
    when ("day7" `elem` args || null args) day7
