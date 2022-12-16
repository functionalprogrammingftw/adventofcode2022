{-# LANGUAGE TupleSections #-}
module Task2Lib (taskFunc) where

import Data.List.Split (splitOn, chunk)
import UtilLib (every, readInt, countTrueGrid, replaceNth)
import Data.List (nub, stripPrefix, insert, intercalate, elemIndex)
import qualified Data.Set (Set, empty, union, fromList, delete, intersection, map, toList)
import Data.Char (ord)
import Data.Maybe (mapMaybe)
import Control.Monad.State (State, MonadState (get, put))

type Coord = (Int, Int)
type MinMax = (Int, Int)

taskFunc :: [String] -> IO ()
taskFunc inputLines = do
    putStrLn "Input data:"
    let inputData = parseInputLines inputLines
    print inputData
    putStrLn "Coordinates where beacons possible:"
    let coord = calculateCoordinates 0 4000000 0 inputData
    print coord
    putStrLn "Tuning frequency:"
    let (x, y) = coord
    print (x * 4000000 + y)

parseInputLines :: [String] -> [(Coord, Coord, Int)]
parseInputLines = map parseInputLine

parseInputLine :: String -> (Coord, Coord, Int)
parseInputLine inputLine = (sensorCoord, beaconCoord, distance)
    where [sensorCoordStr, beaconCoordStr] = splitOn ": closest beacon is at x=" $ drop 12 inputLine
          sensorCoord = parseCoordString sensorCoordStr
          beaconCoord = parseCoordString beaconCoordStr
          distance = calculateDistance sensorCoord beaconCoord

parseCoordString :: String -> Coord
parseCoordString str = (UtilLib.readInt xStr, UtilLib.readInt yStr)
    where [xStr, yStr] = splitOn ", y=" str

calculateDistance :: Coord -> Coord -> Int
calculateDistance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

calculateCoordinates :: Int -> Int -> Int -> [(Coord, Coord, Int)] -> Coord
calculateCoordinates minXY maxXY y coordsAndDistances
    | minX == minXY && maxX == maxXY = calculateCoordinates minXY maxXY (y + 1) coordsAndDistances
    | otherwise = if minX /= minXY then (minX - 1, y) else (maxX + 1, y)
    where ((minX, maxX):_) = calculateXsWhereDistanceEqualOrLess minXY maxXY y coordsAndDistances

calculateXsWhereDistanceEqualOrLess :: Int -> Int -> Int -> [(Coord, Coord, Int)] -> [MinMax]
calculateXsWhereDistanceEqualOrLess minX maxX y = foldl (calculateXsWhereDistanceEqualOrLessFold minX maxX y) []

calculateXsWhereDistanceEqualOrLessFold :: Int -> Int -> Int -> [MinMax] -> (Coord, Coord, Int) -> [MinMax]
calculateXsWhereDistanceEqualOrLessFold minX maxX y minMaxList ((sensorX, sensorY), _, distance)
    | maxXDistance >= 0 = addToMinMaxList 0 minMaxList (max minX $ sensorX - maxXDistance, min maxX $ sensorX + maxXDistance)
    | otherwise = minMaxList
    where maxXDistance = distance - abs (sensorY - y)

addToMinMaxList :: Int -> [MinMax] -> MinMax -> [MinMax]
addToMinMaxList index minMaxList minMax
    | index >= length minMaxList = minMax:minMaxList
    | otherwise = case maybeNewMinMax of
        Just newMinMax -> addToMinMaxList 0 (take index minMaxList ++ drop (index + 1) minMaxList) newMinMax
        _ -> addToMinMaxList (index + 1) minMaxList minMax
    where maybeNewMinMax = mergeMinMaxes minMax (minMaxList !! index)

mergeMinMaxes :: MinMax -> MinMax -> Maybe MinMax
mergeMinMaxes (minX1, maxX1) (minX2, maxX2)
    | minX1 > maxX2 || maxX1 < minX2 = Nothing
    | otherwise = Just (min minX1 minX2, max maxX1 maxX2)
