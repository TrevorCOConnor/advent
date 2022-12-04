module Main where

-- Standard
import System.Environment
import Control.Monad

-- Local
import Day1
import Day2
import Day3
import Day4
import Data.Char (toLower)

main :: IO ()
main = do
    args' <- getArgs
    let args = map (map toLower) args'
    when ("day1" `elem` args) day1
    when ("day2" `elem` args) day2
    when ("day3" `elem` args) day3
    when ("day4" `elem` args) day4
