{-# LANGUAGE NamedFieldPuns #-}
module Task1Lib (taskFunc) where

import Data.List.Split (splitOn, chunk)
import UtilLib (every, readInt, countTrueGrid, replaceNth)
import Data.List (nub, stripPrefix, insert, intercalate, elemIndex)
import qualified Data.Map (Map, empty, lookup, insert, foldr)
import Data.Char (ord)

taskFunc :: [String] -> IO ()
taskFunc inputLines = do
    putStrLn "Input lines:"
    print inputLines
    putStrLn "Packet data pairs:"
    let pairs = parseInputLines inputLines
    printPairs pairs

data PacketData = PDInt Int | PDList [PacketData] deriving (Show, Eq)

printPairs :: [(PacketData, PacketData)] -> IO ()
printPairs [pair] = print pair
printPairs (pair:pairs) = do
    print pair
    printPairs pairs

parseInputLines :: [String] -> [(PacketData, PacketData)]
parseInputLines inputLines = map parseInputLinePair inputLinePairs
    where inputLinePairs = splitOn [""] inputLines

parseInputLinePair :: [String] -> (PacketData, PacketData)
parseInputLinePair [line1, line2] = (parseInputLine line1, parseInputLine line2)

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
