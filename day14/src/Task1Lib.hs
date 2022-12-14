module Task1Lib (taskFunc) where

import Data.List.Split (splitOn, chunk)
import UtilLib (every, readInt, countTrueGrid, replaceNth)
import Data.List (nub, stripPrefix, insert, intercalate, elemIndex)
import qualified Data.Map (Map, empty, lookup, insert, foldr)
import Data.Char (ord)
import Data.Maybe (fromJust)
import Data.Map (Map, delete, elems)
import Control.Monad.State (State, MonadState (get, put))

taskFunc :: [String] -> IO ()
taskFunc inputLines = do
    putStrLn "Input coordinate lists:"
    let coordLists = parseInputLines inputLines
    print coordLists
    putStrLn "Initial material map:"
    let materialMap = generateInitialMaterialMap coordLists
    print materialMap
    putStrLn "Final material map:"
    let finalMaterialMap = addSandUntilFinished materialMap
    print finalMaterialMap
    putStrLn "Sand count:"
    let count = length $ filter (== Sand) $ Data.Map.elems finalMaterialMap
    print count

data Material = Rock | Sand deriving (Show, Eq)
type Coord = (Int, Int)
type MaterialMap = Map Coord Material

addSandUntilFinished :: MaterialMap -> MaterialMap
addSandUntilFinished materialMap = case addSand materialMap of
    (newMaterialMap, False) -> addSandUntilFinished newMaterialMap
    (newMaterialMap, True) -> newMaterialMap

addSand :: MaterialMap -> (MaterialMap, Bool)
addSand materialMap = case Data.Map.lookup startCoord materialMap of 
    Just _ -> (materialMap, True)
    Nothing -> moveSand materialMap startCoord
    where startCoord = (500, 0)

moveSand :: MaterialMap -> Coord -> (MaterialMap, Bool)
moveSand materialMap (sandX, sandY) = case (downMaterial, downLeftMaterial, downRightMaterial, sandY) of 
    (_, _, _, 10000) -> (sandRemovedMaterialMap, True)
    (Nothing, _, _, _) -> moveSand (Data.Map.insert downCoord Sand sandRemovedMaterialMap) downCoord
    (_, Nothing, _, _) -> moveSand (Data.Map.insert downLeftCoord Sand sandRemovedMaterialMap) downLeftCoord
    (_, _, Nothing, _) -> moveSand (Data.Map.insert downRightCoord Sand sandRemovedMaterialMap) downRightCoord
    (Just _, Just _, Just _, _) -> (materialMap, False)
    where downCoord =  (sandX, sandY + 1)
          downLeftCoord = (sandX - 1, sandY + 1)
          downRightCoord = (sandX + 1, sandY + 1)
          downMaterial = Data.Map.lookup downCoord materialMap
          downLeftMaterial = Data.Map.lookup downLeftCoord materialMap
          downRightMaterial = Data.Map.lookup downRightCoord materialMap
          sandRemovedMaterialMap = Data.Map.delete (sandX, sandY) materialMap

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
