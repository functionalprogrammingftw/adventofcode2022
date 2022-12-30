module Task1Lib (taskFunc) where

import Control.Monad.State (MonadState (get, put), State)
import Data.Char (ord)
import qualified Data.HashSet (HashSet, delete, empty, fromList, insert, isSubsetOf, member, singleton, union)
import Data.List (any, elemIndex, foldl', insert, intercalate, nub, stripPrefix, findIndex)
import Data.List.Split (chunk, split, splitOn)
import qualified Data.Map (Map, empty, insert, lookup)
import Data.Maybe (catMaybes, fromJust)
import UtilLib (anyIndexed, countTrueGrid, every, filterIndexed, readInt, replaceNth)

taskFunc :: [String] -> IO ()
taskFunc inputLines = do
  let inputData = parseInputLines inputLines
  putStrLn "Input data:"
  print inputData

parseInputLines :: [String] -> (ExpressionMap, NumberMap)
parseInputLines = foldl' parseInputLinesFold (Data.Map.empty, Data.Map.empty)

parseInputLinesFold :: (ExpressionMap, NumberMap) -> String -> (ExpressionMap, NumberMap)
parseInputLinesFold (expressionMap, numberMap) inputLine
  | length lineEndSplit == 3 = (newExpressionMap, numberMap)
  | otherwise = (expressionMap, newNumberMap)
  where [monkeyName, lineEnd] = splitOn ": " inputLine
        lineEndSplit = splitOn " " lineEnd
        newExpressionMap = Data.Map.insert monkeyName (head lineEndSplit, lineEndSplit !! 1, last lineEndSplit) expressionMap
        newNumberMap = Data.Map.insert monkeyName (UtilLib.readInt $ head lineEndSplit) numberMap

type MonkeyName = String
type Operator = String
type Expression = (MonkeyName, Operator, MonkeyName)
type ExpressionMap = Data.Map.Map MonkeyName Expression
type NumberMap = Data.Map.Map MonkeyName Int
