{-# LANGUAGE ViewPatterns #-}
module Task1Lib (taskFunc) where

import Data.List.Split (splitOn)
import UtilLib (every, readInt, countTrueGrid)
import Data.List (nub, stripPrefix, insert, intercalate)
import qualified Data.Map (Map, empty, lookup, insert, foldr)

taskFunc :: [String] -> IO ()
taskFunc inputLines = do
    putStrLn "Tree grid:"
    let treeGrid = parseInputLines inputLines
    print treeGrid
    putStrLn "Visibility grid:"
    let visibilityGrid = calcAllVisible treeGrid treeGrid 0
    print visibilityGrid
    putStrLn "Number of visible trees:"
    let count = UtilLib.countTrueGrid visibilityGrid
    print count

parseInputLines :: [[Char]] -> [[Int]]
parseInputLines = map parseInputLine

parseInputLine :: [Char] -> [Int]
parseInputLine = map (\c -> readInt [c])

calcAllVisible :: [[Int]] -> [[Int]] -> Int -> [[Bool]]
calcAllVisible _ [] _ = []
calcAllVisible treeGrid (treeRow:treeRows) y = calcAllRowVisible treeGrid treeRow (0, y):calcAllVisible treeGrid treeRows (y + 1)

calcAllRowVisible :: [[Int]] -> [Int] -> (Int, Int) -> [Bool]
calcAllRowVisible treeGrid [] _ = []
calcAllRowVisible treeGrid (tree:trees) (x, y) =
    (calcTreeVisible treeRow tree x || calcTreeVisible treeColumn tree y):calcAllRowVisible treeGrid trees (x + 1, y)
    where treeRow = treeGrid !! y
          treeColumn = [treeRow !! x | treeRow <- treeGrid]

calcTreeVisible :: [Int] -> Int -> Int -> Bool
calcTreeVisible trees tree 0 = True
calcTreeVisible trees tree pos =
    allTreesLower (take pos trees) tree ||
    allTreesLower (drop (pos + 1) trees) tree

allTreesLower :: [Int] -> Int -> Bool
allTreesLower [] _ = True
allTreesLower (tree:trees) height
    | tree < height = allTreesLower trees height
    | otherwise = False