module Task2Lib (taskFunc) where

import Control.Monad.State (MonadState (get, put), State)
import Data.Char (ord)
import qualified Data.HashSet (HashSet, delete, empty, fromList, insert, isSubsetOf, member, singleton, union)
import Data.List (any, elemIndex, findIndex, foldl', insert, intercalate, nub, stripPrefix)
import Data.List.Split (chunk, split, splitOn)
import qualified Data.Map (Map, empty, filterWithKey, insert, lookup, notMember, toList)
import Data.Maybe (catMaybes, fromJust)
import UtilLib (anyIndexed, countTrueGrid, every, filterIndexed, readInt, replaceNth)

taskFunc :: [String] -> IO ()
taskFunc inputLines = do
  let inputData = parseInputLines inputLines
  putStrLn "Input data:"
  print inputData
  -- let updatedMaps = updateMaps inputData
  -- putStrLn "Updated maps:"
  -- print updatedMaps
  let result = calcResult inputData
  putStrLn "Result:"
  print result

parseInputLines :: [String] -> (ExpressionMap, NumberMap)
parseInputLines = foldl' parseInputLinesFold (Data.Map.empty, Data.Map.empty)

parseInputLinesFold :: (ExpressionMap, NumberMap) -> String -> (ExpressionMap, NumberMap)
parseInputLinesFold (expressionMap, numberMap) inputLine
  | length lineEndSplit == 3 = (newExpressionMap, numberMap)
  | otherwise = (expressionMap, newNumberMap)
  where
    [monkeyName, lineEnd] = splitOn ": " inputLine
    lineEndSplit = splitOn " " lineEnd
    newExpressionMap = Data.Map.insert monkeyName (head lineEndSplit, lineEndSplit !! 1, last lineEndSplit) expressionMap
    newNumberMap = Data.Map.insert monkeyName (UtilLib.readInt $ head lineEndSplit) numberMap

calcResult :: (ExpressionMap, NumberMap) -> Int
calcResult (expressionMap, numberMap) = case Data.Map.lookup "root" numberMap of
  Just rootNumber -> rootNumber
  _ -> calcResult $ updateMaps (expressionMap, numberMap)

updateMaps :: (ExpressionMap, NumberMap) -> (ExpressionMap, NumberMap)
updateMaps (expressionMap, numberMap) = (newExpressionMap, newNumberMap)
  where
    newNumberMap = updateNumberMap (Data.Map.toList expressionMap) numberMap
    newExpressionMap = Data.Map.filterWithKey (\monkeyName _ -> Data.Map.notMember monkeyName newNumberMap) expressionMap

updateNumberMap :: [(MonkeyName, Expression)] -> NumberMap -> NumberMap
updateNumberMap [] numberMap = numberMap
updateNumberMap ((monkeyName, (exprMonkeyName1, operator, exprMonkeyName2)) : expressionTuples) numberMap = updateNumberMap expressionTuples newNumberMap
  where
    newNumberMap = case (Data.Map.lookup exprMonkeyName1 numberMap, Data.Map.lookup exprMonkeyName2 numberMap) of
      (Just number1, Just number2) -> Data.Map.insert monkeyName (performCalculation number1 operator number2) numberMap
      _ -> numberMap

performCalculation :: Int -> String -> Int -> Int
performCalculation number1 operator number2
  | operator == "+" = number1 + number2
  | operator == "-" = number1 - number2
  | operator == "*" = number1 * number2
  | operator == "/" = number1 `div` number2

type MonkeyName = String

type Operator = String

type Expression = (MonkeyName, Operator, MonkeyName)

type ExpressionMap = Data.Map.Map MonkeyName Expression

type NumberMap = Data.Map.Map MonkeyName Int
