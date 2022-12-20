module Task2Lib (taskFunc) where

import Control.Monad.State (MonadState (get, put), State)
import Data.Char (ord)
import Data.List (elemIndex, filter, insert, intercalate, nub, stripPrefix)
import Data.List.Split (chunk, splitOn)
import qualified Data.Map (Map, empty, insert, lookup)
import Data.Maybe (fromJust)
import qualified Data.Set (Set, delete, empty, fromList, insert, isSubsetOf, singleton, union, toList)
import UtilLib (countTrueGrid, every, readInt, replaceNth)

taskFunc :: [String] -> IO ()
taskFunc inputLines = do
  putStrLn "Cubes:"
  let cubes = parseInputLines inputLines
  print cubes
  putStrLn "Cube Set:"
  let cubeSet = Data.Set.fromList cubes
  print cubeSet
  putStrLn "Min coord:"
  let minCoord = (minimum $ map (\(x, y, z) -> x - 1) cubes, minimum $ map (\(x, y, z) -> y - 1) cubes, minimum $ map (\(x, y, z) -> z - 1) cubes)
  print minCoord
  let maxCoord = (maximum $ map (\(x, y, z) -> x + 1) cubes, maximum $ map (\(x, y, z) -> y + 1) cubes, maximum $ map (\(x, y, z) -> z + 1) cubes)
  putStrLn "Max coord:"
  print maxCoord
  putStrLn "Outside cubes:"
  let outsideCubeSet = findOutsideCubes minCoord maxCoord cubeSet
  print outsideCubeSet
  putStrLn "Sum of uncovered sides:"
  let sumUncovered = sum $ map (countExposedSides outsideCubeSet) cubes
  print sumUncovered

type Coord = (Int, Int, Int)
parseInputLines :: [String] -> [Coord]
parseInputLines = map parseInputLine

parseInputLine :: String -> Coord
parseInputLine str = (UtilLib.readInt xStr, UtilLib.readInt yStr, UtilLib.readInt zStr)
  where
    [xStr, yStr, zStr] = splitOn "," str

findOutsideCubes :: Coord -> Coord -> Data.Set.Set Coord -> Data.Set.Set Coord
findOutsideCubes minCoord maxCoord cubeSet = findOutsideCubesSub minCoord maxCoord cubeSet (Data.Set.singleton minCoord) [minCoord]

findOutsideCubesSub :: Coord -> Coord -> Data.Set.Set Coord -> Data.Set.Set Coord -> [Coord] -> Data.Set.Set Coord
findOutsideCubesSub minCoord maxCoord cubeSet outsideCubeSet toVisitCubes =
  if null accessibleNeighbors then outsideCubeSet else findOutsideCubesSub minCoord maxCoord cubeSet newOutsideCubeSet newToVisitCubes
  where accessibleNeighbors = Data.Set.fromList $ concatMap (getAccessibleNeighbors minCoord maxCoord cubeSet outsideCubeSet) toVisitCubes
        newOutsideCubeSet = Data.Set.union outsideCubeSet accessibleNeighbors
        newToVisitCubes = Data.Set.toList accessibleNeighbors

getAccessibleNeighbors :: Coord -> Coord -> Data.Set.Set Coord -> Data.Set.Set Coord -> Coord -> [Coord]
getAccessibleNeighbors (minX, minY, minZ) (maxX, maxY, maxZ) cubeSet outsideCubeSet (x, y, z) =
  Data.List.filter
    ( \neighbor@(x, y, z) ->
        x >= minX
          && x <= maxX
          && y >= minY
          && y <= maxY
          && z >= minZ
          && z <= maxZ
          && neighbor `notElem` cubeSet
          && neighbor `notElem` outsideCubeSet
    )
    neighborCubes
  where
    neighborCubes = [(x - 1, y, z), (x + 1, y, z), (x, y - 1, z), (x, y + 1, z), (x, y, z - 1), (x, y, z + 1)]

countExposedSides :: Data.Set.Set Coord -> Coord -> Int
countExposedSides outsideCubeSet (x, y, z) =
  fromEnum ((x + 1, y, z) `elem` outsideCubeSet)
    + fromEnum ((x - 1, y, z) `elem` outsideCubeSet)
    + fromEnum ((x, y + 1, z) `elem` outsideCubeSet)
    + fromEnum ((x, y - 1, z) `elem` outsideCubeSet)
    + fromEnum ((x, y, z + 1) `elem` outsideCubeSet)
    + fromEnum ((x, y, z - 1) `elem` outsideCubeSet)