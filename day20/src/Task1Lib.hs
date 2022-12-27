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
  let numbers = parseInputLines inputLines
  putStrLn "Numbers:"
  print numbers
  putStrLn "Numbers length:"
  print $ length numbers
  -- let rearrangedNumbers1 = rearrangeNumber numbers (numbers !! 0) 
  -- putStrLn "Rearranged numbers:"
  -- print rearrangedNumbers1
  -- let rearrangedNumbers2 = rearrangeNumber rearrangedNumbers1 (numbers !! 1) 
  -- putStrLn "Rearranged numbers:"
  -- print rearrangedNumbers2
  -- let rearrangedNumbers3 = rearrangeNumber rearrangedNumbers2 (numbers !! 2) 
  -- putStrLn "Rearranged numbers:"
  -- print rearrangedNumbers3
  let rearrangedNumbers = rearrangeNumbers numbers 
  putStrLn "Rearranged numbers:"
  print rearrangedNumbers
  let coordinates@(x, y, z) = findCoordinates rearrangedNumbers 
  putStrLn "Coordinates:"
  print coordinates
  putStrLn "Sum:"
  print (x + y + z)

parseInputLines :: [String] -> [Int]
parseInputLines = map parseInputLine

parseInputLine :: String -> Int
parseInputLine = UtilLib.readInt

findCoordinates :: [Int] -> (Int, Int, Int)
findCoordinates numbers = (x, y, z)
  where numbersLength = length numbers
        zeroIndex = fromJust $ elemIndex 0 numbers
        x = numbers !! ((zeroIndex + 1000) `mod` numbersLength)
        y = numbers !! ((zeroIndex + 2000) `mod` numbersLength)
        z = numbers !! ((zeroIndex + 3000) `mod` numbersLength)

rearrangeNumbers :: [Int] -> [Int]
rearrangeNumbers numbers = foldl rearrangeNumber numbers numbers

rearrangeNumber :: [Int] -> Int -> [Int]
rearrangeNumber numbers number = take newNumberIdx numbersRemoved ++ [number] ++ drop newNumberIdx numbersRemoved
  where numbersLength = length numbers
        numberIdx = fromJust $ elemIndex number numbers
        potentialNumberIdx = numberIdx + number
        newNumberIdx
          | potentialNumberIdx <= 0 = addUntilPositive potentialNumberIdx (numbersLength - 1)
          | potentialNumberIdx >= numbersLength - 1 = potentialNumberIdx `mod` (numbersLength - 1)
          | otherwise = potentialNumberIdx
        numbersRemoved = take numberIdx numbers ++ drop (numberIdx + 1) numbers

addUntilPositive :: Int -> Int -> Int
addUntilPositive number toAdd
  | result > 0 = result
  | otherwise = addUntilPositive result toAdd
  where result = number + toAdd