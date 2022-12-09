module Task2Lib (taskFunc) where

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
    let state = calcPositions initialPositions commands
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

ropeLength :: Int
ropeLength = 10

initialPositions :: [Position]
initialPositions = [Position $ replicate ropeLength (0, 0)]

calcUniqueTailPosList :: [Position] -> [(Int, Int)]
calcUniqueTailPosList positions = nub $ map (\(Position list) -> list !! (ropeLength - 1)) positions

calcPositions :: [Position] -> [(Char, Int)] -> [Position]
calcPositions = foldl calcPositionsSingleCommand

calcPositionsSingleCommand :: [Position] -> (Char, Int) -> [Position]
calcPositionsSingleCommand positions (_, 0) = positions
calcPositionsSingleCommand positions (move, count) = calcPositionsSingleCommand (calcPositionSingleMove (head positions) move:positions) (move, count - 1)

calcPositionSingleMove :: Position -> Char -> Position
calcPositionSingleMove (Position ((x, y):coords)) 'R' = calcPositionConsequencesSingleMove (Position ((x + 1, y):coords))
calcPositionSingleMove (Position ((x, y):coords)) 'L' = calcPositionConsequencesSingleMove (Position ((x - 1, y):coords))
calcPositionSingleMove (Position ((x, y):coords)) 'D' = calcPositionConsequencesSingleMove (Position ((x, y + 1):coords))
calcPositionSingleMove (Position ((x, y):coords)) 'U' = calcPositionConsequencesSingleMove (Position ((x, y - 1):coords))

calcPositionConsequencesSingleMove :: Position -> Position
calcPositionConsequencesSingleMove (Position [updatedCoord]) = Position [updatedCoord]
calcPositionConsequencesSingleMove (Position (updatedCoord:nextCoord:coords)) = Position (updatedCoord:updatedCoords)
    where updatedNextCoord = calcUpdatedNextCoord updatedCoord nextCoord
          Position updatedCoords = calcPositionConsequencesSingleMove (Position (updatedNextCoord:coords))

calcUpdatedNextCoord :: (Int, Int) -> (Int, Int) -> (Int, Int)
calcUpdatedNextCoord (updatedX, updatedY) (nextX, nextY)
    | updatedX - nextX > 1 = (nextX + 1, calcCalibratedNextOther updatedY nextY)
    | updatedX - nextX < -1 = (nextX - 1, calcCalibratedNextOther updatedY nextY)
    | updatedY - nextY > 1 = (calcCalibratedNextOther updatedX nextX, nextY + 1)
    | updatedY - nextY < -1 = (calcCalibratedNextOther updatedX nextX, nextY - 1)
    | otherwise = (nextX, nextY)

calcCalibratedNextOther :: Int -> Int -> Int
calcCalibratedNextOther updated next
    | updated > next = next + 1
    | updated < next = next - 1
    | otherwise = next

newtype Position = Position [(Int, Int)]
    deriving (Show, Eq)
