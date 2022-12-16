module Task1Lib (taskFunc) where

import Data.List.Split (splitOn, chunk)
import UtilLib (every, readInt, countTrueGrid, replaceNth)
import Data.List (nub, stripPrefix, insert, intercalate, elemIndex)
import qualified Data.Set (Set, empty, union, fromList, delete)
import Data.Char (ord)
import Data.Maybe (fromJust)
import Control.Monad.State (State, MonadState (get, put))

type Coord = (Int, Int)

taskFunc :: [String] -> IO ()
taskFunc inputLines = do
    putStrLn "Input data:"
    let inputData = parseInputLines inputLines
    print inputData
    putStrLn "Xs where beacons not possible:"
    let xsWhereBeaconNotPossible = calculateXsWhereBeaconNotPossible 2000000 inputData
    print xsWhereBeaconNotPossible
    putStrLn "Count:"
    let count = length xsWhereBeaconNotPossible
    print count

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

calculateXsWhereBeaconNotPossible :: Int -> [(Coord, Coord, Int)] -> Data.Set.Set Int
calculateXsWhereBeaconNotPossible y = foldl (calculateXsWhereBeaconNotPossibleSingleFold y) Data.Set.empty

calculateXsWhereBeaconNotPossibleSingleFold :: Int -> Data.Set.Set Int -> (Coord, Coord, Int) -> Data.Set.Set Int
calculateXsWhereBeaconNotPossibleSingleFold y prevSet (sensorCoord, (beaconX, beaconY), distance) =
    if y == beaconY then Data.Set.delete beaconX xsWhereDistanceEqualOrLess else xsWhereDistanceEqualOrLess
    where xsWhereDistanceEqualOrLess = Data.Set.union prevSet $ calculateXsWhereDistanceEqualOrLess y sensorCoord distance

calculateXsWhereDistanceEqualOrLess :: Int -> Coord -> Int -> Data.Set.Set Int
calculateXsWhereDistanceEqualOrLess y1 (x2, y2) distance = Data.Set.fromList [x2 - maxXDistance..x2 + maxXDistance]
    where maxXDistance = distance - abs (y1 - y2)
