module Task1Lib (taskFunc) where

import Data.List.Split (splitOn, chunk)
import UtilLib (every, readInt, countTrueGrid, replaceNth)
import Data.List (nub, stripPrefix, insert, intercalate, elemIndex)
import qualified Data.Map (Map, empty, lookup, insert, foldr)
import Data.Char (ord)
import Data.Maybe (fromJust)
import Data.Map (Map)
import Control.Monad.State (State, MonadState (get, put))

taskFunc :: [String] -> IO ()
taskFunc inputLines = do
    putStrLn "Input coordinate lists:"
    let coordLists = parseInputLines inputLines
    print coordLists
    putStrLn "Initial material map:"
    let materialMap = generateInitialMaterialMap coordLists
    print materialMap

data Material = Rock | Sand deriving (Show, Eq)
type Coord = (Int, Int)
type MaterialMap = Map Coord Material

addSand :: MaterialMap -> MaterialMap
addSand materialMap = case Data.Map.lookup startCoord materialMap of 
    Just _ -> materialMap
    Nothing -> newMaterialMap
    where startCoord = (500, 0)
          newMaterialMap = Data.Map.insert startCoord Sand materialMap


generateInitialMaterialMap :: [[Coord]] -> MaterialMap
generateInitialMaterialMap = foldl updateMaterialMapFromCoordList Data.Map.empty

updateMaterialMapFromCoordList :: MaterialMap -> [Coord] -> MaterialMap
updateMaterialMapFromCoordList materialMap coordList = foldl updateMaterialMapFromCoordPair materialMap coordPairList
    where coordPairList = zip coordList $ tail coordList

updateMaterialMapFromCoordPair :: MaterialMap -> (Coord, Coord) -> MaterialMap
updateMaterialMapFromCoordPair materialMap ((x1, y1), (x2, y2))
    | x1 < x2 && y1 == y2 = updateMaterialMapFromCoordPair newMaterialMap ((x1 + 1, y1), (x2, y2))
    | x1 > x2 && y1 == y2 = updateMaterialMapFromCoordPair newMaterialMap ((x1 - 1, y1), (x2, y2))
    | x1 == x2 && y1 < y2 = updateMaterialMapFromCoordPair newMaterialMap ((x1, y1 + 1), (x2, y2))
    | x1 == x2 && y1 > y2 = updateMaterialMapFromCoordPair newMaterialMap ((x1, y1 - 1), (x2, y2))
    | x1 == x2 && y1 == y2 = newMaterialMap
        where newMaterialMap = Data.Map.insert (x1, y1) Rock materialMap

parseInputLines :: [String] -> [[Coord]]
parseInputLines = map parseInputLine

parseInputLine :: String -> [Coord]
parseInputLine inputLine = map parseCoordString $ splitOn " -> " inputLine

parseCoordString :: String -> Coord
parseCoordString coordStr = (UtilLib.readInt xStr, UtilLib.readInt yStr)
    where [xStr, yStr] = splitOn "," coordStr
