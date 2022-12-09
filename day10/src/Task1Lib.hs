module Task1Lib (taskFunc) where

import Data.List.Split (splitOn)
import UtilLib (every, readInt, countTrueGrid)
import Data.List (nub, stripPrefix, insert, intercalate)
import qualified Data.Map (Map, empty, lookup, insert, foldr)

taskFunc :: [String] -> IO ()
taskFunc inputLines = do
    putStrLn "Input lines:"
    print inputLines








parseInputLines :: [String] -> [(Char, Int)]
parseInputLines [] = []
parseInputLines (line:lines) = command:parseInputLines lines
    where command = (head $ head splitted, readInt $ last splitted)
          splitted = splitOn " " line

calcUniqueTailPosList :: [Position] -> [(Int, Int)]

calcUniqueTailPosList positions = nub $ map tailPos positions

calcTailPosList :: [Position] -> [(Char, Int)] -> [Position]

calcTailPosList = foldl calcTailPosListSingleCommand

calcTailPosListSingleCommand :: [Position] -> (Char, Int) -> [Position]

calcTailPosListSingleCommand positions (_, 0) = positions

calcTailPosListSingleCommand (position:positions) ('R', count) =
    calcTailPosListSingleCommand (newPosition:position:positions) ('R', count - 1)
    where (Position (headX, headY) (tailX, tailY)) = position
          newHeadX = headX + 1
          newHeadY = headY
          newHeadPos = (newHeadX, newHeadY)
          tailShouldMove = calcTailShouldMove newHeadPos (tailX, tailY)
          newTailX
            | tailShouldMove = tailX + 1
            | otherwise = tailX
          newTailY
            | tailShouldMove && tailY < headY = tailY + 1
            | tailShouldMove && tailY > headY = tailY - 1
            | otherwise = tailY
          newTailPos = (newTailX, newTailY)
          newPosition = Position newHeadPos newTailPos

calcTailPosListSingleCommand (position:positions) ('L', count) =
    calcTailPosListSingleCommand (newPosition:position:positions) ('L', count - 1)
    where (Position (headX, headY) (tailX, tailY)) = position
          newHeadX = headX - 1
          newHeadY = headY
          newHeadPos = (newHeadX, newHeadY)
          tailShouldMove = calcTailShouldMove newHeadPos (tailX, tailY)
          newTailX
            | tailShouldMove = tailX - 1
            | otherwise = tailX
          newTailY
            | tailShouldMove && tailY < headY = tailY + 1
            | tailShouldMove && tailY > headY = tailY - 1
            | otherwise = tailY
          newTailPos = (newTailX, newTailY)
          newPosition = Position newHeadPos newTailPos

calcTailPosListSingleCommand (position:positions) ('D', count) =
    calcTailPosListSingleCommand (newPosition:position:positions) ('D', count - 1)
    where (Position (headX, headY) (tailX, tailY)) = position
          newHeadX = headX
          newHeadY = headY + 1
          newHeadPos = (newHeadX, newHeadY)
          tailShouldMove = calcTailShouldMove newHeadPos (tailX, tailY)
          newTailX
            | tailShouldMove && tailX < headX = tailX + 1
            | tailShouldMove && tailX > headX = tailX - 1
            | otherwise = tailX
          newTailY
            | tailShouldMove = tailY + 1
            | otherwise = tailY
          newTailPos = (newTailX, newTailY)
          newPosition = Position newHeadPos newTailPos

calcTailPosListSingleCommand (position:positions) ('U', count) =
    calcTailPosListSingleCommand (newPosition:position:positions) ('U', count - 1)
    where (Position (headX, headY) (tailX, tailY)) = position
          newHeadX = headX
          newHeadY = headY - 1
          newHeadPos = (newHeadX, newHeadY)
          tailShouldMove = calcTailShouldMove newHeadPos (tailX, tailY)
          newTailX
            | tailShouldMove && tailX < headX = tailX + 1
            | tailShouldMove && tailX > headX = tailX - 1
            | otherwise = tailX
          newTailY
            | tailShouldMove = tailY - 1
            | otherwise = tailY
          newTailPos = (newTailX, newTailY)
          newPosition = Position newHeadPos newTailPos

calcTailShouldMove :: (Int, Int) -> (Int, Int) -> Bool

calcTailShouldMove (newHeadX, newHeadY) (tailX, tailY) = abs (newHeadX - tailX) > 1 || abs (newHeadY - tailY) > 1

data Position = Position {headPos :: (Int, Int), tailPos :: (Int, Int)}
    deriving (Show, Eq)
