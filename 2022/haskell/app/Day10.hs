module Day10 (day10) where

import Data.List.Split
import Control.Monad
import Control.Concurrent (threadDelay)
import System.IO


fp :: FilePath
fp = "../data/Day10.txt"


data CycleRegister = CR { cycleNumber :: Int, register :: Int}
    deriving (Show)


data Registry = Registry { currentState :: CycleRegister
                         , history :: [CycleRegister]
                         }


newCycle :: CycleRegister
newCycle = CR 0 1


cycleUp :: CycleRegister -> CycleRegister
cycleUp cr = cr { cycleNumber = cycleNumber cr + 1 }


addRegister :: Int -> CycleRegister -> CycleRegister
addRegister reg cr = cr { register = register cr + reg }


applyCommand :: Registry -> String -> Registry
applyCommand registry "noop" = Registry cr' $ cr': history registry
    where cr' = cycleUp $ currentState registry

applyCommand registry line = Registry (addRegister reg cr'') $
                             cr'' : cr' : history registry
    where cr' = cycleUp $ currentState registry
          cr'' = cycleUp cr'
          reg = case splitOn " " line of
              ["addx", reg] -> read reg
              _ -> error "unrecognized input"


signalStrength :: CycleRegister -> Int
signalStrength cr = cycleNumber cr * register cr


pixel :: CycleRegister -> Char
pixel cr = if abs (register cr - ((cycleNumber cr - 1) `rem` 40)) <= 1
              then '#'
              else ' '


-- For fun!
printLive :: [CycleRegister] -> IO()
printLive crs = do
    hSetBuffering stdout NoBuffering
    forM_ crs $ \cr -> do
        threadDelay 7500
        if cycleNumber cr `mod` 40 == 0
               then putStrLn [pixel cr]
               else putChar $ pixel cr


processPrint :: [CycleRegister] -> String
processPrint [] = []
processPrint (cr: crs) = if cycleNumber cr `mod` 40 == 0
               then pixel cr : '\n' : processPrint crs
               else pixel cr : processPrint crs


day10 :: IO ()
day10 = do
    contents <- readFile fp
    let
        cycles = reverse $ history $
            foldl applyCommand (Registry newCycle [newCycle]) $ lines contents
        part1 = sum $ map (signalStrength . (cycles !!)) [20, 60, 100, 140, 180, 220]
        -- part2 = printLive
    putStrLn "Day 10:"
    putStrLn $ "Day 1: " ++ show part1
    putStrLn $ "Day 2:\n" ++ processPrint (tail cycles)
    -- putStrLn "Day 2: "
    -- printLive $ tail cycles
