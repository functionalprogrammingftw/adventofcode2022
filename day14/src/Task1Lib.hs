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
    print $ length materialMap

data Material = Rock | Sand deriving (Show, Eq)
type Coord = (Int, Int)
type MaterialMap = Map Coord Material

generateInitialMaterialMap :: [[Coord]] -> MaterialMap
generateInitialMaterialMap = foldl updateMaterialMapFromCoordList Data.Map.empty

updateMaterialMapFromCoordList :: MaterialMap -> [Coord] -> MaterialMap
updateMaterialMapFromCoordList materialMap [coord] = Data.Map.insert coord Rock materialMap
updateMaterialMapFromCoordList materialMap (coord1:coord2:coords) = updateMaterialMapFromCoordList newMaterialMap (coord2:coords)
    where newMaterialMap = updateMaterialMapFromCoordPair materialMap coord1 coord2

updateMaterialMapFromCoordPair :: MaterialMap -> Coord -> Coord -> MaterialMap
updateMaterialMapFromCoordPair materialMap (x1, y1) (x2, y2)
    | x1 < x2 && y1 == y2 = updateMaterialMapFromCoordPair newMaterialMap (x1 + 1, y1) (x2, y2)
    | x1 > x2 && y1 == y2 = updateMaterialMapFromCoordPair newMaterialMap (x1 - 1, y1) (x2, y2)
    | x1 == x2 && y1 < y2 = updateMaterialMapFromCoordPair newMaterialMap (x1, y1 + 1) (x2, y2)
    | x1 == x2 && y1 > y2 = updateMaterialMapFromCoordPair newMaterialMap (x1, y1 - 1) (x2, y2)
    | x1 == x2 && y1 == y2 = materialMap
        where newMaterialMap = Data.Map.insert (x1, y1) Rock materialMap

parseInputLines :: [String] -> [[Coord]]
parseInputLines = map parseInputLine

parseInputLine :: String -> [Coord]
parseInputLine inputLine = map parseCoordString $ splitOn " -> " inputLine

parseCoordString :: String -> Coord
parseCoordString coordStr = (UtilLib.readInt xStr, UtilLib.readInt yStr)
    where [xStr, yStr] = splitOn "," coordStr
