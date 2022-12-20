module Task1Lib (taskFunc) where

import Control.Monad.State (MonadState (get, put), State)
import Data.Char (ord)
import Data.List (elemIndex, insert, intercalate, nub, stripPrefix)
import Data.List.Split (chunk, splitOn)
import qualified Data.Map (Map, empty, insert, lookup)
import Data.Maybe (fromJust)
import qualified Data.Set (Set, delete, empty, fromList, insert, isSubsetOf, singleton, union)
import UtilLib (countTrueGrid, every, readInt, replaceNth)

taskFunc :: [String] -> IO ()
taskFunc inputLines = do
  putStrLn "Cubes:"
  let cubes = parseInputLines inputLines
  print cubes
  putStrLn "Cube Set:"
  let cubeSet = Data.Set.fromList cubes
  print cubeSet
  putStrLn "Sum of uncovered sides:"
  let sumUncovered = sum $ map (countExposedSides cubeSet) cubes
  print sumUncovered

parseInputLines :: [String] -> [(Int, Int, Int)]
parseInputLines = map parseInputLine

parseInputLine :: String -> (Int, Int, Int)
parseInputLine str = (UtilLib.readInt xStr, UtilLib.readInt yStr, UtilLib.readInt zStr)
  where
    [xStr, yStr, zStr] = splitOn "," str

countExposedSides :: Data.Set.Set (Int, Int, Int) -> (Int, Int, Int) -> Int
countExposedSides cubeSet (x, y, z) =
  fromEnum ((x + 1, y, z) `notElem` cubeSet)
    + fromEnum ((x - 1, y, z) `notElem` cubeSet)
    + fromEnum ((x, y + 1, z) `notElem` cubeSet)
    + fromEnum ((x, y - 1, z) `notElem` cubeSet)
    + fromEnum ((x, y, z + 1) `notElem` cubeSet)
    + fromEnum ((x, y, z - 1) `notElem` cubeSet)