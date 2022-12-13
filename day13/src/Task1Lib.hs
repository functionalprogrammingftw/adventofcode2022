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
    print pairs

data PacketData = PDInt Int | PDList [PacketData] deriving (Show, Eq)

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
    ('[':restStr) -> parsePDList str (PDList [])
    (c:restStr) -> parsePDInt str 0

parsePDList :: String -> PacketData -> (String, PacketData)
parsePDList str (PDList currList) = case str of
    (']':restStr) -> (restStr, PDList currList)
    (',':restStr) -> parsePDList restStr2 (PDList (currList ++ [packetData]))
        where (restStr2, packetData) = parsePacketData restStr
    (c:_) -> parsePDList restStr2 (PDList (currList ++ [packetData]))
        where (restStr2, packetData) = parsePacketData str

parsePDInt :: String -> Int -> (String, PacketData)
parsePDInt str currInt = case str of
    (']':_) -> (str, PDInt currInt)
    (',':_) -> (str, PDInt currInt)
    (c:restStr) -> parsePDInt restStr (currInt * 10 + UtilLib.readInt [c])
