module Main where

-- Standard
import System.Environment
import Control.Monad

-- Local
import Day1
import Day2

main :: IO ()
main = do
    args <- getArgs
    when ("Day1" `elem` args) day1
    when ("Day2" `elem` args) day2
