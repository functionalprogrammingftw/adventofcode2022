module Task2Lib (taskFunc) where

import Data.List.Split (splitOn, chunk)
import UtilLib (every, readInt, countTrueGrid, replaceNth)
import Data.List (nub, stripPrefix, insert, intercalate, elemIndex, sortBy)
import qualified Data.Map (Map, empty, lookup, insert, foldr)
import Data.Char (ord)
import Data.Maybe (fromJust)

taskFunc :: [String] -> IO ()
taskFunc inputLines = do
    -- putStrLn "Input lines:"
    -- print inputLines
    let dividerPacket1 = PDList [PDList [PDInt 2]]
    let dividerPacket2 = PDList [PDList [PDInt 6]]
    -- putStrLn "Packet data list:"
    let packetDataList = dividerPacket1:dividerPacket2:parseInputLines inputLines
    -- print packetDataList
    -- putStrLn "Sorted packet data list:"
    let sortedPacketDataList = sortBy comparePacketData packetDataList
    -- print sortedPacketDataList
    putStrLn "Divider packet indices:"
    let index1 = fromJust (elemIndex dividerPacket1 sortedPacketDataList) + 1
    let index2 = fromJust (elemIndex dividerPacket2 sortedPacketDataList) + 1
    print index1
    print index2
    putStrLn "Indices multiplied:"
    print $ index1 * index2

data PacketData = PDInt Int | PDList [PacketData] deriving (Show, Eq)

printPairs :: [(PacketData, PacketData)] -> IO ()
printPairs [pair] = print pair
printPairs (pair:pairs) = do
    print pair
    printPairs pairs

sumTrueIndices :: [Bool] -> Int
sumTrueIndices bools = sum [ index | (index, bool) <- zip [1..] bools, bool ]

comparePacketData :: PacketData -> PacketData -> Ordering
comparePacketData leftPacketData rightPacketData = case comparePacketDataPair (leftPacketData, rightPacketData) of
    Just False -> GT
    Just True -> LT
    Nothing -> EQ

comparePacketDataPair :: (PacketData, PacketData) -> Maybe Bool
comparePacketDataPair (PDInt leftInt, PDInt rightInt)
    | leftInt < rightInt = Just True
    | leftInt > rightInt = Just False
    | otherwise = Nothing
comparePacketDataPair (PDList [], PDList []) = Nothing
comparePacketDataPair (PDList [], PDList (_:_)) = Just True
comparePacketDataPair (PDList (_:_), PDList []) = Just False
comparePacketDataPair (PDList (leftPacketData:leftRestList), PDList (rightPacketData:rightRestList)) =
    case comparePacketDataPair (leftPacketData, rightPacketData) of
        Nothing -> comparePacketDataPair (PDList leftRestList, PDList rightRestList)
        justB -> justB
comparePacketDataPair (PDList leftList, PDInt rightInt) = comparePacketDataPair (PDList leftList, PDList [PDInt rightInt])
comparePacketDataPair (PDInt leftInt, PDList rightList) = comparePacketDataPair (PDList [PDInt leftInt], PDList rightList)

parseInputLines :: [String] -> [PacketData]
parseInputLines inputLines = map parseInputLine $ filter (/= "") inputLines

parseInputLine :: String -> PacketData
parseInputLine ('[':str) = packetData
    where (_, packetData) = parsePacketData ('[':str)

parsePacketData :: String -> (String, PacketData)
parsePacketData str = case str of
    ('[':restStr) -> parsePDList restStr (PDList [])
    (c:_) -> parsePDInt str (PDInt 0)

parsePDList :: String -> PacketData -> (String, PacketData)
parsePDList str currPdList = case (str, currPdList) of
    (']':restStr, _) -> (restStr, currPdList)
    (',':restStr, PDList currList) -> parsePDList restStr2 (PDList (currList ++ [packetData]))
        where (restStr2, packetData) = parsePacketData restStr
    (_, PDList currList) -> parsePDList restStr2 (PDList (currList ++ [packetData]))
        where (restStr2, packetData) = parsePacketData str

parsePDInt :: String -> PacketData -> (String, PacketData)
parsePDInt str currPdInt = case (str, currPdInt) of
    (']':_, _) -> (str, currPdInt)
    (',':_, _) -> (str, currPdInt)
    (c:restStr, PDInt currInt) -> parsePDInt restStr (PDInt (currInt * 10 + UtilLib.readInt [c]))
