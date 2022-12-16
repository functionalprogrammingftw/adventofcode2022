{-# LANGUAGE TupleSections #-}
module Task2Lib (taskFunc) where

import Data.List.Split (splitOn, chunk)
import UtilLib (every, readInt, countTrueGrid, replaceNth)
import Data.List (nub, stripPrefix, insert, intercalate, elemIndex)
import qualified Data.Set (Set, empty, union, fromList, delete, intersection, map, toList)
import Data.Char (ord)
import Data.Maybe (fromJust)
import Control.Monad.State (State, MonadState (get, put))

type Coord = (Int, Int)

taskFunc :: [String] -> IO ()
taskFunc inputLines = do
    putStrLn "Input data:"
    let inputData = parseInputLines inputLines
    print inputData
    putStrLn "Coordinates where beacons possible:"
    let coords = calculateCoordinates 0 20 inputData
    print coords
    putStrLn "Tuning frequency:"
    let (x, y) = head $ Data.Set.toList coords
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

calculateCoordinates :: Int -> Int -> [(Coord, Coord, Int)] -> Data.Set.Set Coord
calculateCoordinates minXY maxXY coordsAndDistance =
    foldl1 Data.Set.union $ map (calculateCoordsWhereBeaconPossible minXY maxXY coordsAndDistance) [minXY..maxXY]

calculateCoordsWhereBeaconPossible :: Int -> Int -> [(Coord, Coord, Int)] -> Int -> Data.Set.Set Coord
calculateCoordsWhereBeaconPossible minX maxX coordsAndDistance y = Data.Set.map (, y) xs
    where xs = foldl1 Data.Set.intersection $ map (calculateXsWhereDistanceGreater minX maxX y) coordsAndDistance

calculateXsWhereDistanceGreater :: Int -> Int -> Int -> (Coord, Coord, Int) -> Data.Set.Set Int
calculateXsWhereDistanceGreater minX maxX y ((sensorX, sensorY), _, distance) =
    Data.Set.fromList ([minX..sensorX - minXDistance] ++ [sensorX + minXDistance..maxX])
    where minXDistance = distance - abs (sensorY - y) + 1
