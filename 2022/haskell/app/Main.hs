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
import Day8
import Day9
import Day10
import Day11
import Day12
import Day13
import Day14
import Day15
import Day16
import Day17
import Day18
import Day19
import Day20
import Day21
import Day22
import Day23
import Day24
import Day25

main :: IO ()
main = do
    args' <- getArgs
    let args = map (map toLower) args'
    when ("day1"  `elem` args || null args) day1
    when ("day2"  `elem` args || null args) day2
    when ("day3"  `elem` args || null args) day3
    when ("day4"  `elem` args || null args) day4
    when ("day5"  `elem` args || null args) day5
    when ("day6"  `elem` args || null args) day6
    when ("day7"  `elem` args || null args) day7
    when ("day8"  `elem` args || null args) day8
    when ("day9"  `elem` args || null args) day9
    when ("day10" `elem` args || null args) day10
    when ("day11" `elem` args || null args) day11
    when ("day12" `elem` args || null args) day12
    when ("day13" `elem` args || null args) day13
    when ("day14" `elem` args || null args) day14
    when ("day15" `elem` args || null args) day15
    when ("day16" `elem` args || null args) day16
    when ("day17" `elem` args || null args) day17
    when ("day18" `elem` args || null args) day18
    when ("day19" `elem` args || null args) day19
    when ("day20" `elem` args || null args) day20
    when ("day21" `elem` args || null args) day21
    when ("day22" `elem` args || null args) day22
    when ("day23" `elem` args || null args) day23
    when ("day24" `elem` args || null args) day24
    when ("day25" `elem` args || null args) day25
