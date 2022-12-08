{-# LANGUAGE ViewPatterns #-}
module Task2Lib (taskFunc) where

import Data.List.Split (splitOn)
import UtilLib (every, readInt, countTrueGrid)
import Data.List (nub, stripPrefix, insert, intercalate)
import qualified Data.Map (Map, empty, lookup, insert, foldr)

taskFunc :: [String] -> IO ()
taskFunc inputLines = do
    putStrLn "Tree grid:"
    let treeGrid = parseInputLines inputLines
    print treeGrid
    putStrLn "Scenic score grid:"
    let scenicScoreGrid = calcAllScenicScore treeGrid treeGrid 0
    print scenicScoreGrid
    putStrLn "Max scenic score:"
    let max = maximum $ map maximum scenicScoreGrid
    print max

parseInputLines :: [[Char]] -> [[Int]]
parseInputLines = map parseInputLine

parseInputLine :: [Char] -> [Int]
parseInputLine = map (\c -> readInt [c])

calcAllScenicScore :: [[Int]] -> [[Int]] -> Int -> [[Int]]
calcAllScenicScore _ [] _ = []
calcAllScenicScore treeGrid (treeRow:treeRows) y = calcAllRowScenicScore treeGrid treeRow (0, y):calcAllScenicScore treeGrid treeRows (y + 1)

calcAllRowScenicScore :: [[Int]] -> [Int] -> (Int, Int) -> [Int]
calcAllRowScenicScore treeGrid [] _ = []
calcAllRowScenicScore treeGrid (tree:trees) (x, y) =
    (calcTreeScenicScore treeRow tree x * calcTreeScenicScore treeColumn tree y):calcAllRowScenicScore treeGrid trees (x + 1, y)
    where treeRow = treeGrid !! y
          treeColumn = [treeRow !! x | treeRow <- treeGrid]

calcTreeScenicScore :: [Int] -> Int -> Int -> Int
calcTreeScenicScore trees tree pos =
    calcScenicScore (reverse $ take pos trees) tree *
    calcScenicScore (drop (pos + 1) trees) tree

calcScenicScore :: [Int] -> Int -> Int
calcScenicScore [] _ = 0
calcScenicScore (tree:trees) height
    | tree < height = 1 + calcScenicScore trees height
    | otherwise = 1