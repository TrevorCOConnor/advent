module Main where

-- Standard
import System.Environment
import Control.Monad

-- Local
import Day1

main :: IO ()
main = do
    args <- getArgs
    when ("Day1" `elem` args) day1
