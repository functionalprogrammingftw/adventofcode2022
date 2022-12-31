module Task2Lib (taskFunc) where

import Control.Monad.State (MonadState (get, put), State)
import Data.Char (ord)
import qualified Data.HashSet (HashSet, delete, empty, fromList, insert, isSubsetOf, member, singleton, union)
import Data.List (any, elemIndex, find, findIndex, foldl', insert, intercalate, nub, stripPrefix)
import Data.List.Split (chunk, split, splitOn)
import qualified Data.Map (Map, delete, empty, filterWithKey, insert, lookup, notMember, toList, unionWith)
import Data.Maybe (catMaybes, fromJust)
import UtilLib (anyIndexed, countTrueGrid, every, filterIndexed, readInt, replaceNth)

taskFunc :: [String] -> IO ()
taskFunc inputLines = do
  let inputData@(expressionMap, numberMap) = parseInputLines inputLines
  putStrLn "Number map:"
  print numberMap
  putStrLn "Expression map:"
  print expressionMap
  let updatedExpressionMapSingle = updateExpressionMapSingle expressionMap Data.Map.empty myName
  putStrLn "Updated expression map single:"
  print updatedExpressionMapSingle
  updatedExpressionMap <- updateExpressionMap expressionMap
  putStrLn "Updated expression map:"
  print updatedExpressionMap
  result <- calcResult (updatedExpressionMap, numberMap)
  putStrLn "Result:"
  print result

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

updateExpressionMap :: ExpressionMap -> IO ExpressionMap
updateExpressionMap expressionMap = updateExpressionMapMultiple expressionMap Data.Map.empty [myName]

updateExpressionMapMultiple :: ExpressionMap -> ExpressionMap -> [MonkeyName] -> IO ExpressionMap
updateExpressionMapMultiple expressionMap buildExpressionMap [] = do
  putStrLn "Called with expression maps and no names"
  print expressionMap
  print buildExpressionMap
  putStrLn "Finished"
  return $ Data.Map.unionWith (\e1 e2 -> undefined) expressionMap buildExpressionMap
updateExpressionMapMultiple expressionMap buildExpressionMap (resolveMonkeyName : resolveMonkeyNames) = do
  putStrLn "Called with expression maps and names"
  print expressionMap
  print buildExpressionMap
  print (resolveMonkeyName : resolveMonkeyNames)
  updateExpressionMapMultiple newExpressionMap newBuildExpressionMap (resolveMonkeyNames ++ newResolveMonkeyNames)
  where
    (newExpressionMap, newBuildExpressionMap, newResolveMonkeyNames) = updateExpressionMapSingle expressionMap buildExpressionMap resolveMonkeyName

updateExpressionMapSingle :: ExpressionMap -> ExpressionMap -> MonkeyName -> (ExpressionMap, ExpressionMap, [MonkeyName])
updateExpressionMapSingle expressionMap buildExpressionMap resolveMonkeyName = case findResult of
  Just item -> updateExpressionMapSingleFound expressionMap buildExpressionMap resolveMonkeyName item
  Nothing -> (expressionMap, buildExpressionMap, [])
  where
    expressions = Data.Map.toList expressionMap
    findResult = find (\(monkeyName, (name1, _, name2)) -> name1 == resolveMonkeyName || name2 == resolveMonkeyName) expressions

updateExpressionMapSingleFound :: ExpressionMap -> ExpressionMap -> MonkeyName -> (MonkeyName, Expression) -> (ExpressionMap, ExpressionMap, [MonkeyName])
updateExpressionMapSingleFound expressionMap buildExpressionMap resolveMonkeyName (monkeyName, (exprMonkeyName1, "+", exprMonkeyName2))
  | resolveMonkeyName == exprMonkeyName1 = (newExpressionMap, Data.Map.insert resolveMonkeyName (monkeyName, "-", exprMonkeyName2) buildExpressionMap, [monkeyName])
  | otherwise = (newExpressionMap, Data.Map.insert resolveMonkeyName (monkeyName, "-", exprMonkeyName1) buildExpressionMap, [monkeyName])
  where
    newExpressionMap = Data.Map.delete monkeyName expressionMap
updateExpressionMapSingleFound expressionMap buildExpressionMap resolveMonkeyName (monkeyName, (exprMonkeyName1, "-", exprMonkeyName2))
  | resolveMonkeyName == exprMonkeyName1 = (newExpressionMap, Data.Map.insert resolveMonkeyName (monkeyName, "+", exprMonkeyName2) buildExpressionMap, [monkeyName])
  | otherwise = (newExpressionMap, Data.Map.insert resolveMonkeyName (exprMonkeyName1, "-", monkeyName) buildExpressionMap, [monkeyName])
  where
    newExpressionMap = Data.Map.delete monkeyName expressionMap
updateExpressionMapSingleFound expressionMap buildExpressionMap resolveMonkeyName (monkeyName, (exprMonkeyName1, "*", exprMonkeyName2))
  | resolveMonkeyName == exprMonkeyName1 = (newExpressionMap, Data.Map.insert resolveMonkeyName (monkeyName, "/", exprMonkeyName2) buildExpressionMap, [monkeyName])
  | otherwise = (newExpressionMap, Data.Map.insert resolveMonkeyName (monkeyName, "/", exprMonkeyName1) buildExpressionMap, [monkeyName])
  where
    newExpressionMap = Data.Map.delete monkeyName expressionMap
updateExpressionMapSingleFound expressionMap buildExpressionMap resolveMonkeyName (monkeyName, (exprMonkeyName1, "/", exprMonkeyName2))
  | resolveMonkeyName == exprMonkeyName1 = (newExpressionMap, Data.Map.insert resolveMonkeyName (monkeyName, "*", exprMonkeyName2) buildExpressionMap, [monkeyName])
  | otherwise = (newExpressionMap, Data.Map.insert resolveMonkeyName (exprMonkeyName1, "/", monkeyName) buildExpressionMap, [monkeyName])
  where
    newExpressionMap = Data.Map.delete monkeyName expressionMap
updateExpressionMapSingleFound expressionMap buildExpressionMap resolveMonkeyName (monkeyName, (exprMonkeyName1, "=", exprMonkeyName2))
  | resolveMonkeyName == exprMonkeyName1 = (newExpressionMap, Data.Map.insert resolveMonkeyName (exprMonkeyName2, "+", zeroName) buildExpressionMap, [])
  | otherwise = (newExpressionMap, Data.Map.insert resolveMonkeyName (exprMonkeyName1, "+", zeroName) buildExpressionMap, [])
  where
    newExpressionMap = Data.Map.delete monkeyName expressionMap

updateExpressionMapSingleFoundMult :: ExpressionMap -> MonkeyName -> (MonkeyName, Expression) -> (ExpressionMap, [MonkeyName])
updateExpressionMapSingleFoundMult expressionMap resolveMonkeyName (monkeyName, (exprMonkeyName1, operator, exprMonkeyName2)) = undefined

updateExpressionMapSingleFoundDiv :: ExpressionMap -> MonkeyName -> (MonkeyName, Expression) -> (ExpressionMap, [MonkeyName])
updateExpressionMapSingleFoundDiv expressionMap resolveMonkeyName (monkeyName, (exprMonkeyName1, operator, exprMonkeyName2)) = undefined

updateExpressionMapSingleFoundEq :: ExpressionMap -> MonkeyName -> (MonkeyName, Expression) -> (ExpressionMap, [MonkeyName])
updateExpressionMapSingleFoundEq expressionMap resolveMonkeyName (monkeyName, (exprMonkeyName1, operator, exprMonkeyName2)) = undefined

calcResult :: (ExpressionMap, NumberMap) -> IO Int
calcResult (expressionMap, numberMap) = do
  putStrLn "Calculation iteration:"
  print expressionMap
  print numberMap
  case Data.Map.lookup myName numberMap of
    Just myNumber -> return myNumber
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
