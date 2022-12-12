{-# LANGUAGE NamedFieldPuns #-}
module Task1Lib (taskFunc) where

import Data.List.Split (splitOn, chunk)
import UtilLib (every, readInt, countTrueGrid, replaceNth)
import Data.List (nub, stripPrefix, insert, intercalate, elemIndex)
import qualified Data.Map (Map, empty, lookup, insert, foldr)
import Data.Char (ord)

taskFunc :: [String] -> IO ()
taskFunc inputLines = do
    putStrLn "Height map:"
    let heightMap = parseInputLines inputLines
    print heightMap
    putStrLn "Test:"
    let unseenAccessible = getUnseenAccessibleCoords heightMap [Coord (1, 3)] (Coord (2, 3))
    print unseenAccessible
    putStrLn "Step count:"
    let stepCount = calcStepCount heightMap
    print stepCount

newtype Coord = Coord (Int, Int) deriving (Show, Eq)

data HeightMap = HeightMap {
    array :: [[Int]],
    startCoord :: Coord,
    endCoord :: Coord
} deriving (Show, Eq)

parseInputLines :: [String] -> HeightMap
parseInputLines inputLines = HeightMap{array, startCoord, endCoord}
    where array = map (map parseChar) inputLines
          startCoord = findCoord inputLines 0 'S'
          endCoord = findCoord inputLines 0 'E'

parseChar :: Char -> Int
parseChar c = case c of
    'S' -> parseChar 'a'
    'E' -> parseChar 'z'
    _ -> ord c - ord 'a'

findCoord :: [String] -> Int -> Char -> Coord
findCoord (inputLine:inputLines) y c = case elemIndex c inputLine of
    Just x -> Coord (x, y)
    _ -> findCoord inputLines (y + 1) c

calcStepCount :: HeightMap -> Int
calcStepCount heightMap = performSteps heightMap [] [startCoord heightMap] 0 

performSteps :: HeightMap -> [Coord] -> [Coord] -> Int -> Int
performSteps heightMap seenCoords toVisitCoords stepNo
    | endSeen = newStepNo
    | otherwise = performSteps heightMap newSeenCoords newToVisitCoords newStepNo
    where (newSeenCoords, newToVisitCoords, endSeen) = performStep heightMap seenCoords toVisitCoords
          newStepNo = stepNo + 1

performStep :: HeightMap -> [Coord] -> [Coord] -> ([Coord], [Coord], Bool)
performStep heightMap seenCoords toVisitCoords = (newSeenCoords, newToVisitCoords, endSeen)
    where (newSeenCoords, newToVisitCoords) = visitCoords heightMap seenCoords [] toVisitCoords
          endSeen = endCoord heightMap `elem` newSeenCoords

visitCoords :: HeightMap -> [Coord] -> [Coord] -> [Coord] -> ([Coord], [Coord])
visitCoords heightMap seenCoords newToVisitCoords [] = (seenCoords, newToVisitCoords)
visitCoords heightMap seenCoords newToVisitCoords (visitCoord:remainingVisitCoords) =
    visitCoords heightMap (seenCoords ++ unseenAccessibleCoords) (newToVisitCoords ++ unseenAccessibleCoords) remainingVisitCoords
    where unseenAccessibleCoords = getUnseenAccessibleCoords heightMap seenCoords visitCoord

getUnseenAccessibleCoords :: HeightMap -> [Coord] -> Coord -> [Coord]
getUnseenAccessibleCoords heightMap seenCoords (Coord (x, y)) = filter (`notElem` seenCoords) (accessibleUp ++ accessibleDown ++ accessibleLeft ++ accessibleRight)
    where row = array heightMap !! y
          column = [ row !! x | row <- array heightMap]
          elevation = row !! x
          accessibleUp = [ Coord (x, y - 1) | y > 0 && column !! (y - 1) <= elevation + 1 ]
          accessibleDown = [ Coord (x, y + 1) | y + 1 < length column && column !! (y + 1) <= elevation + 1 ]
          accessibleLeft = [ Coord (x - 1, y) | x > 0 && row !! (x - 1) <= elevation + 1 ]
          accessibleRight = [ Coord (x + 1, y) | x + 1 < length row && row !! (x + 1) <= elevation + 1 ]
