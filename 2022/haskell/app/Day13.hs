module Day13 (day13) where

import Data.List
import Data.List.Split
import Data.Aeson
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy.UTF8 as BLU
import Data.Maybe


fp :: FilePath
fp = "../data/Day13.txt"


makePacket :: String -> Value
makePacket = fromJust . decode . BLU.fromString


packetOrder :: Value -> Value -> Ordering
packetOrder left right =
    case _checkPacketOrder left right of
      Nothing -> EQ
      Just True -> LT
      Just False -> GT


_checkPacketOrder :: Value -> Value -> Maybe Bool
_checkPacketOrder (Number x) (Number y) = if x == y then Nothing else Just (x < y)
_checkPacketOrder (Array l) (Array r) = _checkVector l r
_checkPacketOrder l@(Number _) r@(Array _) = _checkPacketOrder (Array (V.singleton l)) r
_checkPacketOrder l@(Array _) r@(Number _) = _checkPacketOrder l (Array (V.singleton r))
_checkPacketOrder _ _ = error "Unexpected format for packets"


_checkVector :: V.Vector Value -> V.Vector Value -> Maybe Bool
_checkVector left right
  | V.null left && V.null right = Nothing
  | V.null left = Just True
  | V.null right = Just False
  | otherwise = case _checkPacketOrder (V.head left) (V.head right) of
                  Nothing -> _checkVector (V.tail left) (V.tail right)
                  result -> result


take2 :: [a] -> (a, a)
take2 as = (head as, as !! 1)


day13 :: IO ()
day13 = do
    contents <- readFile fp
    let packetPairs = map (map makePacket) $ splitOn [""] $ lines contents
    let
        part1 = sum $ map fst $ filter (uncurry (<) . take2 . snd) $ zip [1..] packetPairs
        sortedPackets = sortBy packetOrder $ concat packetPairs ++ map makePacket ["[[2]]", "[[6]]"]
        firstIndex = (1+) <$> elemIndex (makePacket "[[2]]") sortedPackets
        secondIndex = (1+) <$> elemIndex (makePacket "[[6]]") sortedPackets
        part2 = fromJust $ fmap (*) firstIndex <*> secondIndex
    putStrLn "Day 13:"
    putStrLn $ "Day 1: " ++ show part1
    putStrLn $ "Day 2: " ++ show part2
