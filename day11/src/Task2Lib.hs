{-# LANGUAGE NamedFieldPuns #-}
module Task2Lib (taskFunc) where

import Data.List.Split (splitOn, chunk)
import UtilLib (every, readInt, readInt, countTrueGrid, replaceNth, fpow)
import Data.List (nub, stripPrefix, insert, intercalate, sort)
import qualified Data.Map (Map, empty, lookup, insert, foldr)

taskFunc :: [String] -> IO ()
taskFunc inputLines = do
    putStrLn "Initial monkey states:"
    let monkeyStates = parseInputLines inputLines
    print monkeyStates
    putStrLn "Monkey states after one round:"
    print $ processRound monkeyStates
    putStrLn "Monkey states after 10000 rounds:"
    let monkeyStatesAfterManyRounds = fpow 10000 processRound monkeyStates
    print monkeyStatesAfterManyRounds
    putStrLn "Inspections after 10000 rounds:"
    let inspectionsAfterManyRounds = map inspections monkeyStatesAfterManyRounds
    print inspectionsAfterManyRounds
    putStrLn "Shenanigans after 10000 rounds:"
    let sorted = reverse $ sort inspectionsAfterManyRounds
    let shenanigans = head sorted * sorted !! 1
    print shenanigans

data MonkeyState = MonkeyState {
    name :: String,
    items :: [Int],
    operation :: String,
    moduloTest :: Int,
    ifTrue :: Int,
    ifFalse :: Int,
    inspections :: Int
} deriving (Show, Eq)

parseInputLines :: [String] -> [MonkeyState]
parseInputLines inputLines = parseInputChunks $ splitOn [""] inputLines

parseInputChunks :: [[String]] -> [MonkeyState]
parseInputChunks = map parseInputChunk

parseInputChunk :: [String] -> MonkeyState
parseInputChunk chunk = MonkeyState{ name, items, operation, moduloTest, ifTrue, ifFalse, inspections }
    where name = init $ head chunk
          items = map UtilLib.readInt (splitOn ", " $ drop 18 $ chunk !! 1)
          operation = drop 23 $ chunk !! 2
          moduloTest = UtilLib.readInt $ drop 21 $ chunk !! 3
          ifTrue = UtilLib.readInt $ drop 29 $ chunk !! 4
          ifFalse = UtilLib.readInt $ drop 30 $ chunk !! 5
          inspections = 0

processRound :: [MonkeyState] -> [MonkeyState]
processRound monkeyStates = processRoundRecursive moduloTestProduct 0 monkeyStates
    where moduloTestProduct = product $ map moduloTest monkeyStates

processRoundRecursive :: Int -> Int -> [MonkeyState] -> [MonkeyState]
processRoundRecursive moduloTestProduct monkeyNo monkeyStates
    | monkeyNo >= length monkeyStates = monkeyStates
    | otherwise = processRoundRecursive moduloTestProduct (monkeyNo + 1) $ processItems moduloTestProduct monkeyNo monkeyStates

processItems :: Int -> Int -> [MonkeyState] -> [MonkeyState]
processItems moduloTestProduct monkeyNo monkeyStates = case monkeyItems of
    [] -> monkeyStates
    _ -> processItems moduloTestProduct monkeyNo $ processItem moduloTestProduct monkeyNo monkeyStates
    where monkeyItems = items $ monkeyStates !! monkeyNo

processItem :: Int -> Int -> [MonkeyState] -> [MonkeyState]
processItem moduloTestProduct monkeyNo monkeyStates = addMonkeyItem newMonkeyItem throwToMonkeyNo newMonkeyStates
    where (monkeyItem, newMonkeyStates) = getMonkeyItem monkeyNo monkeyStates
          monkeyState = newMonkeyStates !! monkeyNo
          monkeyOperation = getMonkeyOperation monkeyState
          newMonkeyItem = calculateNewMonkeyItem moduloTestProduct monkeyOperation monkeyItem
          throwToMonkeyNo
            | newMonkeyItem `mod` moduloTest monkeyState == 0 = ifTrue monkeyState
            | otherwise = ifFalse monkeyState

getMonkeyItem :: Int -> [MonkeyState] -> (Int, [MonkeyState])
getMonkeyItem monkeyNo monkeyStates = (monkeyItem, newMonkeyStates)
    where oldMonkeyState = monkeyStates !! monkeyNo
          monkeyItem = head $ items oldMonkeyState
          newMonkeyItems = tail $ items oldMonkeyState
          newInspections = inspections oldMonkeyState + 1
          newMonkeyState = oldMonkeyState { items = newMonkeyItems, inspections = newInspections }
          newMonkeyStates = UtilLib.replaceNth monkeyNo newMonkeyState monkeyStates

addMonkeyItem :: Int -> Int -> [MonkeyState] -> [MonkeyState]
addMonkeyItem monkeyItem monkeyNo monkeyStates = newMonkeyStates
    where oldMonkeyState = monkeyStates !! monkeyNo
          newMonkeyItems = items oldMonkeyState ++ [monkeyItem]
          newMonkeyState = oldMonkeyState { items = newMonkeyItems}
          newMonkeyStates = UtilLib.replaceNth monkeyNo newMonkeyState monkeyStates

getMonkeyOperation :: MonkeyState -> Int -> Int
getMonkeyOperation monkeyState
    | stringOperand == "old" = \x -> x `realOperator` x
    | otherwise = \x -> x `realOperator` UtilLib.readInt stringOperand
    where [stringOperator, stringOperand] = splitOn " " $ operation monkeyState
          realOperator
            | stringOperator == "*" = (*)
            | otherwise = (+)

calculateNewMonkeyItem :: Int -> (Int -> Int) -> Int -> Int
calculateNewMonkeyItem moduloTestProduct monkeyOperation monkeyItem = monkeyOperation monkeyItem `mod` moduloTestProduct