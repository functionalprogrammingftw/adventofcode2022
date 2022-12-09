{-# LANGUAGE ViewPatterns #-}
module Task1Lib (taskFunc) where

import Data.List.Split (splitOn)
import UtilLib (every, readInt, countTrueGrid)
import Data.List (nub, stripPrefix, insert, intercalate)
import qualified Data.Map (Map, empty, lookup, insert, foldr)

taskFunc :: [String] -> IO ()
taskFunc inputLines = do
    putStrLn "Commands:"
    let commands = parseInputLines inputLines
    print commands
    putStrLn "Calculation result:"
    let state = calcTailPosList (State [(0, 0)] (0, 0) (0, 0)) commands
    print state
    putStrLn "Unique tail position list:"
    let uniqueTailPosList = calcUniqueTailPosList state
    print uniqueTailPosList
    putStrLn "Result:"
    print $ length uniqueTailPosList

parseInputLines :: [String] -> [(Char, Int)]

parseInputLines [] = []

parseInputLines (line:lines) = command:parseInputLines lines
    where command = (head $ head splitted, readInt $ last splitted)
          splitted = splitOn " " line

calcUniqueTailPosList :: State -> [(Int, Int)]

calcUniqueTailPosList (State tailPosList _ _) = nub tailPosList

calcTailPosList :: State -> [(Char, Int)] -> State

calcTailPosList state [] = state

calcTailPosList state (command:commands) = newState
    where newState = calcTailPosList (calcTailPosListSingleCommand state command) commands

calcTailPosListSingleCommand :: State -> (Char, Int) -> State

calcTailPosListSingleCommand state (_, 0) = state

calcTailPosListSingleCommand (State tailPosList (headX, headY) (tailX, tailY)) ('R', count) =
    calcTailPosListSingleCommand (State newTailPosList newHeadPos newTailPos) ('R', count - 1)
    where newHeadPos = (newHeadX, newHeadY)
          newTailPos = (newTailX, newTailY)
          newHeadX = headX + 1
          newHeadY = headY
          newTailX
            | tailX + 1 < newHeadX = tailX + 1
            | otherwise = tailX
          newTailY
            | tailY < headY = tailY + 1
            | tailY > headY = tailY - 1
            | otherwise = tailY
          newTailPosList = newTailPos:tailPosList

calcTailPosListSingleCommand (State tailPosList (headX, headY) (tailX, tailY)) ('L', count) =
    calcTailPosListSingleCommand (State newTailPosList newHeadPos newTailPos) ('L', count - 1)
    where newHeadPos = (newHeadX, newHeadY)
          newTailPos = (newTailX, newTailY)
          newHeadX = headX - 1
          newHeadY = headY
          newTailX
            | tailX - 1 > newHeadX = tailX - 1
            | otherwise = tailX
          newTailY
            | tailY < headY = tailY + 1
            | tailY > headY = tailY - 1
            | otherwise = tailY
          newTailPosList = newTailPos:tailPosList

calcTailPosListSingleCommand (State tailPosList (headX, headY) (tailX, tailY)) ('D', count) =
    calcTailPosListSingleCommand (State newTailPosList newHeadPos newTailPos) ('D', count - 1)
    where newHeadPos = (newHeadX, newHeadY)
          newTailPos = (newTailX, newTailY)
          newHeadX = headX
          newHeadY = headY + 1
          newTailX
            | tailX < headX = tailX + 1
            | tailY > headX = tailX - 1
            | otherwise = tailX
          newTailY
            | tailY + 1 < newHeadY = tailY + 1
            | otherwise = tailY
          newTailPosList = newTailPos:tailPosList

calcTailPosListSingleCommand (State tailPosList (headX, headY) (tailX, tailY)) ('U', count) =
    calcTailPosListSingleCommand (State newTailPosList newHeadPos newTailPos) ('U', count - 1)
    where newHeadPos = (newHeadX, newHeadY)
          newTailPos = (newTailX, newTailY)
          newHeadX = headX
          newHeadY = headY - 1
          newTailX
            | tailX < headX = tailX + 1
            | tailY > headX = tailX - 1
            | otherwise = tailX
          newTailY
            | tailY - 1 > newHeadY = tailY - 1
            | otherwise = tailY
          newTailPosList = newTailPos:tailPosList

data State = State {tailPosList :: [(Int, Int)], headPos :: (Int, Int), tailPos :: (Int, Int)}
    deriving (Show, Eq)