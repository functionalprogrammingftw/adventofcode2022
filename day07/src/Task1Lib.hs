{-# LANGUAGE ViewPatterns #-}
module Task1Lib (taskFunc) where

import Data.List.Split (splitOn)
import UtilLib (every, readInt)
import Data.List (nub, stripPrefix, insert, intercalate)
import qualified Data.Map (Map, empty, lookup, insert, foldr)

taskFunc :: [String] -> IO ()
taskFunc inputLines = do
    putStrLn "Size map:"
    let sizeMap = parseInputLines Data.Map.empty [] inputLines
    print sizeMap
    putStrLn "Sum size small dirs:"
    let sumSizeSmallDirs = calcSumSizeOfSmallDirs sizeMap
    print sumSizeSmallDirs

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

calcSumSizeOfSmallDirs :: Data.Map.Map String Int -> Int

calcSumSizeOfSmallDirs =
    Data.Map.foldr (\fileSize sumSize -> if fileSize <= 100000 then fileSize + sumSize else sumSize) 0

getDirPath :: [String] -> String
getDirPath = intercalate "/" . reverse