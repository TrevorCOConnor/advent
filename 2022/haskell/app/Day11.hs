module Day11 (day11) where

import           Data.List.Split
import           Text.Regex.Posix
import qualified Data.Map as M
import Data.List (sortOn)


type MonkeyMap = M.Map Int (Counter Monkey)
type Stress = Bool


fp :: FilePath
fp = "../data/Day11.txt"


data Counter a = Counter Int a
    deriving (Show)

count :: Int -> Counter a -> Counter a
count plus (Counter c a) = Counter (c + plus) a

newCounter :: a -> Counter a
newCounter = Counter 0

fromCounter :: Counter a -> a
fromCounter (Counter _ a) = a

toCount :: Counter a -> Int
toCount (Counter x _) = x


instance Functor Counter where
    fmap f (Counter x a) = Counter x (f a)


data Monkey = Monkey
    { monkeyNumber    :: Int
    , monkeyItems     :: [Int]
    , monkeyOperation :: Int -> Int
    , monkeyTest      :: Int
    , monkeyTrue      :: Int
    , monkeyFalse     :: Int
    }

instance Show Monkey where
    show monkey = "Monkey "
                ++ show (monkeyNumber monkey)
                ++ " "
                ++ show (monkeyItems monkey)
                ++ show (monkeyTest monkey)

instance Eq Monkey where
    m1 == m2 = monkeyNumber m1 == monkeyNumber m2


createMonkeyMap :: [Monkey] -> MonkeyMap
createMonkeyMap ms = M.fromList $ [(monkeyNumber m, newCounter m) | m <- ms]


parseMonkey :: [String] -> Monkey
parseMonkey [ monkeyLine, itemLine, operationLine
             , testLine, trueLine, falseLine] =
  Monkey { monkeyNumber=read $ monkeyLine =~ "[0-9]"
         , monkeyItems=map read $ getAllTextMatches $ itemLine =~ "[0-9]+"
         , monkeyOperation=parseOperation $ operationLine =~ "old [+*] (old|[0-9]+)"
         , monkeyTest=read $ testLine =~ "[0-9]+"
         , monkeyTrue=read $ trueLine =~ "[0-9]+"
         , monkeyFalse=read $ falseLine =~ "[0-9]+"
         }

parseMonkey _ = error "Unrecognized monkey format"


parseOperation :: String -> (Int -> Int)
parseOperation line = applyArgument arg $ operator op
    where
        ["old", op, arg] = splitOn " " line

        operator :: String -> (Int -> Int -> Int)
        operator "+" = (+)
        operator "*" = (*)
        operator s   = error $ "Operator " ++ s ++ "not recognized"

        applyArgument :: String -> (Int -> Int -> Int) -> (Int -> Int)
        applyArgument "old" f = \x -> x `f` x
        applyArgument num f   = (`f` read num)


-- Turn Related Functions
handleItem :: Int -> Stress -> Monkey -> Int -> (Int, Int)
handleItem space stress monkey item = if passedTest
                          then (monkeyTrue monkey, modItem)
                          else (monkeyFalse monkey, modItem)
    where operated = monkeyOperation monkey item
          newItem = if stress then operated else operated `div` 3
          modItem = newItem `mod` space
          passedTest = modItem `mod` monkeyTest monkey == 0


removeItemsFromMonkey :: Monkey -> Monkey
removeItemsFromMonkey monkey = monkey { monkeyItems=[] }


addItemToMonkey :: Int -> Monkey -> Monkey
addItemToMonkey item monkey = monkey { monkeyItems=monkeyItems monkey ++ [item] }


monkeyTurn :: Int -> Stress -> Counter Monkey -> MonkeyMap -> MonkeyMap
monkeyTurn space stress monkeycounter monkeyMap = foldr passItem newMap items
    where monkey = fromCounter monkeycounter
          items = monkeyItems monkey
          newMap = M.adjust (count (length items) . fmap removeItemsFromMonkey)
                            (monkeyNumber monkey) monkeyMap

          passItem :: Int -> MonkeyMap -> MonkeyMap
          passItem item mm = M.adjust (fmap (addItemToMonkey newItem)) newMonkey mm
            where (newMonkey, newItem) = handleItem space stress monkey item


monkeyRound :: Int -> Stress -> MonkeyMap -> MonkeyMap
monkeyRound space stress mmap = foldl (\mm k -> monkeyTurn space stress (mm M.! k) mm)
                            mmap $ M.keys mmap


day11 :: IO ()
day11 = do
    contents <- readFile fp
    let
        monkeys = map parseMonkey $ splitOn [""] $ lines contents
        monkeyMap = createMonkeyMap monkeys
        space = product $ map monkeyTest monkeys
        part1rounds = iterate (monkeyRound space False) monkeyMap
        part2rounds = iterate (monkeyRound space True) monkeyMap
        part1 = product $ take 2 $ sortOn negate $ map toCount $ M.elems $ part1rounds !! 20
        part2 = product $ take 2 $ sortOn negate $ map toCount $ M.elems $ part2rounds !! 10000
    print monkeys
    putStrLn "Day 11:"
    putStrLn $ "Day 1: " ++ show part1
    putStrLn $ "Day 2: " ++ show part2
