module Task2Lib (taskFunc) where

import Control.Monad.State (MonadState (get, put), State)
import Data.Char (ord)
import qualified Data.HashSet (HashSet, delete, empty, fromList, insert, isSubsetOf, member, singleton, union)
import Data.List (any, elemIndex, find, findIndex, foldl', insert, intercalate, nub, stripPrefix)
import Data.List.Split (chunk, split, splitOn)
import qualified Data.Map (Map, delete, empty, filterWithKey, insert, lookup, notMember, toList)
import Data.Maybe (catMaybes, fromJust)
import UtilLib (anyIndexed, countTrueGrid, every, filterIndexed, readInt, replaceNth)

taskFunc :: [String] -> IO ()
taskFunc inputLines = do
  let inputData@(expressionMap, numberMap) = parseInputLines inputLines
  putStrLn "Number map:"
  print numberMap
  putStrLn "Expression map:"
  print expressionMap
  let updatedExpressionMap = updateExpressionMapSingle expressionMap myName
  putStrLn "Updated expression map:"
  print updatedExpressionMap
  -- let result = calcResult inputData
  -- putStrLn "Result:"
  -- print result

myName :: MonkeyName
myName = "humn"

zeroName :: MonkeyName
zeroName = "zero"

parseInputLines :: [String] -> (ExpressionMap, NumberMap)
parseInputLines inputLines = (expressionMap, Data.Map.insert zeroName 0 numberMap)
  where
    (expressionMap, numberMap) = foldl' parseInputLinesFold (Data.Map.empty, Data.Map.empty) inputLines

parseInputLinesFold :: (ExpressionMap, NumberMap) -> MonkeyName -> (ExpressionMap, NumberMap)
parseInputLinesFold (expressionMap, numberMap) inputLine
  | length lineEndSplit == 3 && monkeyName == "root" = (newRootExpressionMap, numberMap)
  | length lineEndSplit == 3 = (newExpressionMap, numberMap)
  | monkeyName /= myName = (expressionMap, newNumberMap)
  | otherwise = (expressionMap, numberMap)
  where
    [monkeyName, lineEnd] = splitOn ": " inputLine
    lineEndSplit = splitOn " " lineEnd
    newExpressionMap = Data.Map.insert monkeyName (head lineEndSplit, lineEndSplit !! 1, last lineEndSplit) expressionMap
    newNumberMap = Data.Map.insert monkeyName (UtilLib.readInt $ head lineEndSplit) numberMap
    newRootExpressionMap = Data.Map.insert monkeyName (head lineEndSplit, "=", last lineEndSplit) expressionMap

updateExpressionMap :: ExpressionMap -> ExpressionMap
updateExpressionMap expressionMap = updateExpressionMapMultiple expressionMap [myName]

updateExpressionMapMultiple :: ExpressionMap -> [MonkeyName] -> ExpressionMap
updateExpressionMapMultiple expressionMap [] = expressionMap
updateExpressionMapMultiple expressionMap (resolveMonkeyName : resolveMonkeyNames) =
  updateExpressionMapMultiple newExpressionMap (resolveMonkeyNames ++ newResolveMonkeyNames)
  where
    (newExpressionMap, newResolveMonkeyNames) = updateExpressionMapSingle expressionMap resolveMonkeyName

updateExpressionMapSingle :: ExpressionMap -> MonkeyName -> (ExpressionMap, [MonkeyName])
updateExpressionMapSingle expressionMap resolveMonkeyName = case findResult of
  Just item -> updateExpressionMapSingleFound expressionMap resolveMonkeyName item
  Nothing -> (expressionMap, [])
  where
    expressions = Data.Map.toList expressionMap
    findResult = find (\(monkeyName, (name1, _, name2)) -> name1 == resolveMonkeyName || name2 == resolveMonkeyName) expressions

updateExpressionMapSingleFound :: ExpressionMap -> MonkeyName -> (MonkeyName, Expression) -> (ExpressionMap, [MonkeyName])
updateExpressionMapSingleFound expressionMap resolveMonkeyName (monkeyName, (exprMonkeyName1, "+", exprMonkeyName2))
  | resolveMonkeyName == exprMonkeyName1 = (Data.Map.insert resolveMonkeyName (monkeyName, "-", exprMonkeyName2) deleteFromExpressionMap, [monkeyName])
  | otherwise = (Data.Map.insert resolveMonkeyName (monkeyName, "-", exprMonkeyName1) deleteFromExpressionMap, [monkeyName])
  where
    deleteFromExpressionMap = Data.Map.delete monkeyName expressionMap
updateExpressionMapSingleFound expressionMap resolveMonkeyName (monkeyName, (exprMonkeyName1, "-", exprMonkeyName2))
  | resolveMonkeyName == exprMonkeyName1 = (Data.Map.insert resolveMonkeyName (monkeyName, "+", exprMonkeyName2) deleteFromExpressionMap, [monkeyName])
  | otherwise = (Data.Map.insert resolveMonkeyName (exprMonkeyName1, "-", monkeyName) deleteFromExpressionMap, [monkeyName])
  where
    deleteFromExpressionMap = Data.Map.delete monkeyName expressionMap
updateExpressionMapSingleFound expressionMap resolveMonkeyName (monkeyName, (exprMonkeyName1, "*", exprMonkeyName2))
  | resolveMonkeyName == exprMonkeyName1 = (Data.Map.insert resolveMonkeyName (monkeyName, "/", exprMonkeyName2) deleteFromExpressionMap, [monkeyName])
  | otherwise = (Data.Map.insert resolveMonkeyName (monkeyName, "/", exprMonkeyName1) deleteFromExpressionMap, [monkeyName])
  where
    deleteFromExpressionMap = Data.Map.delete monkeyName expressionMap
updateExpressionMapSingleFound expressionMap resolveMonkeyName (monkeyName, (exprMonkeyName1, "/", exprMonkeyName2))
  | resolveMonkeyName == exprMonkeyName1 = (Data.Map.insert resolveMonkeyName (monkeyName, "*", exprMonkeyName2) deleteFromExpressionMap, [monkeyName])
  | otherwise = (Data.Map.insert resolveMonkeyName (exprMonkeyName1, "/", monkeyName) deleteFromExpressionMap, [monkeyName])
  where
    deleteFromExpressionMap = Data.Map.delete monkeyName expressionMap
updateExpressionMapSingleFound expressionMap resolveMonkeyName (monkeyName, (exprMonkeyName1, "=", exprMonkeyName2))
  | resolveMonkeyName == exprMonkeyName1 = (Data.Map.insert resolveMonkeyName (exprMonkeyName2, "+", zeroName) deleteFromExpressionMap, [])
  | otherwise = (Data.Map.insert resolveMonkeyName (exprMonkeyName1, "+", zeroName) deleteFromExpressionMap, [])
  where
    deleteFromExpressionMap = Data.Map.delete monkeyName expressionMap

updateExpressionMapSingleFoundMult :: ExpressionMap -> MonkeyName -> (MonkeyName, Expression) -> (ExpressionMap, [MonkeyName])
updateExpressionMapSingleFoundMult expressionMap resolveMonkeyName (monkeyName, (exprMonkeyName1, operator, exprMonkeyName2)) = undefined

updateExpressionMapSingleFoundDiv :: ExpressionMap -> MonkeyName -> (MonkeyName, Expression) -> (ExpressionMap, [MonkeyName])
updateExpressionMapSingleFoundDiv expressionMap resolveMonkeyName (monkeyName, (exprMonkeyName1, operator, exprMonkeyName2)) = undefined

updateExpressionMapSingleFoundEq :: ExpressionMap -> MonkeyName -> (MonkeyName, Expression) -> (ExpressionMap, [MonkeyName])
updateExpressionMapSingleFoundEq expressionMap resolveMonkeyName (monkeyName, (exprMonkeyName1, operator, exprMonkeyName2)) = undefined

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

performCalculation :: Int -> Operator -> Int -> Int
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
