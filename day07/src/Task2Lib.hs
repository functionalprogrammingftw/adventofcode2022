{-# LANGUAGE ViewPatterns #-}
module Task2Lib (taskFunc) where

import Data.List.Split (splitOn)
import UtilLib (every, readInt)
import Data.List (nub, stripPrefix, insert, intercalate)
import qualified Data.Map (Map, empty, lookup, insert, foldr)
import Data.Maybe (fromJust)

taskFunc :: [String] -> IO ()
taskFunc inputLines = do
    putStrLn "Size map:"
    let sizeMap = parseInputLines Data.Map.empty [] inputLines
    print sizeMap
    putStrLn "Root size:"
    let rootSize = fromJust $ Data.Map.lookup "/" sizeMap
    print rootSize
    putStrLn "Needed space:"
    let neededSize = 30000000 - (70000000 - rootSize)
    print neededSize
    putStrLn "Size smallest dir to delete:"
    let sizeSmallestDir = findSizeOfSmallestDirLargeEnough neededSize sizeMap
    print sizeSmallestDir

parseInputLines :: Data.Map.Map String Int -> [String] -> [String] -> Data.Map.Map String Int

parseInputLines map path [] = map

parseInputLines map path ("$ ls":lines) =
    parseInputLines map path lines

parseInputLines map path ((stripPrefix "dir " -> Just dir):lines) =
    parseInputLines map path lines

parseInputLines map path ("$ cd ..":lines) =
    parseInputLines map (tail path) lines

parseInputLines map path ((stripPrefix "$ cd " -> Just dir):lines) =
    parseInputLines map (dir:path) lines

parseInputLines map path (line:lines) =
    parseInputLines (updateMap map path fileSize) path lines
    where lineWords = words line
          fileSize = readInt $ head lineWords

updateMap :: Data.Map.Map String Int -> [String] -> Int -> Data.Map.Map String Int

updateMap map [] fileSize = map

updateMap map (dir:restPath) fileSize =
    case Data.Map.lookup dirPath map of
        Just sumSize -> updateMap (Data.Map.insert dirPath (sumSize + fileSize) map) restPath fileSize
        Nothing -> updateMap (Data.Map.insert dirPath fileSize map) restPath fileSize
    where dirPath = getDirPath (dir:restPath)

findSizeOfSmallestDirLargeEnough :: Int -> Data.Map.Map String Int -> Int

findSizeOfSmallestDirLargeEnough minSize =
    Data.Map.foldr (\dirSize smallestSize -> if dirSize < smallestSize && dirSize >= minSize then dirSize else smallestSize) 99999999999

getDirPath :: [String] -> String
getDirPath = intercalate "/" . reverse