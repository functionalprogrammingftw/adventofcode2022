{-# LANGUAGE NamedFieldPuns #-}

module Task2Lib (taskFunc) where

import Control.Monad.State (MonadState (get, put), State)
import qualified Data.Bifunctor
import Data.Char (ord)
import Data.List (any, elemIndex, filter, find, insert, intercalate, nub, stripPrefix)
import Data.List.Split (chunk, splitOn)
import qualified Data.Map (Map, elems, empty, insert, lookup, map, singleton, size)
import Data.Maybe (fromJust)
import qualified Data.Set (Set, delete, elems, empty, fromList, insert, isSubsetOf, map, singleton, size, union)
import UtilLib (countTrueGrid, every, readInt, replaceNth)
import Control.Applicative (Alternative(empty))

type ValveName = String

type ValveMap = Data.Map.Map ValveName Valve

newtype ValveNames = ValveNames (ValveName, ValveName) deriving (Show)

instance Eq ValveNames where
  ValveNames (valveName11, valveName12) == ValveNames (valveName21, valveName22) = concat1 == concat2
    where
      concat1 = if valveName11 < valveName12 then valveName11 ++ valveName12 else valveName12 ++ valveName11
      concat2 = if valveName21 < valveName22 then valveName21 ++ valveName22 else valveName22 ++ valveName21

instance Ord ValveNames where
  ValveNames (valveName11, valveName12) `compare` ValveNames (valveName21, valveName22) = concat1 `compare` concat2
    where
      concat1 = if valveName11 < valveName12 then valveName11 ++ valveName12 else valveName12 ++ valveName11
      concat2 = if valveName21 < valveName22 then valveName21 ++ valveName22 else valveName22 ++ valveName21

data Valve = Valve
  { flowRate :: Int,
    tunnelValves :: [String]
  }
  deriving (Eq, Show)

data Position = Position
  { valveNames :: ValveNames,
    openValves :: Data.Set.Set ValveName,
    openedFlowRate :: Int,
    totalFlow :: Int,
    positionsSinceLastOpen :: (Data.Set.Set ValveName, Data.Set.Set ValveName),
    path :: [(ValveName, ValveName, Data.Set.Set ValveName, Int, Int)]
  }
  deriving (Eq, Show)

type SeenPositionMap = Data.Map.Map ValveNames [(Data.Set.Set ValveName, Int)]

taskFunc :: [String] -> IO ()
taskFunc inputLines = do
  putStrLn "Valve map:"
  let valveMap = parseInputLines inputLines
  print valveMap
  putStrLn "Max total flow:"
  let maxFlowRate = calcMaxFlowRate valveMap
  print maxFlowRate
  putStrLn "Max total flow reachable:"
  let maxFlowRateReachable = calcMaxFlowRateReachable valveMap $ Data.Set.singleton "AA"
  print maxFlowRateReachable
  putStrLn "Position length:"
  let (positions, seenPositionsMap) = handleSteps valveMap maxFlowRate 10
  print $ length positions
  putStrLn "Seen Positions Map:"
  print seenPositionsMap
  putStrLn "Opened flow rate:"
  let maxPressure = maximum $ map openedFlowRate positions
  print maxPressure
  putStrLn "Maximum pressure:"
  let maxPressure = maximum $ map totalFlow positions
  print maxPressure

-- putStrLn "Test:"
-- print $ filter (\position -> totalFlow position == maxPressure) positions

parseInputLines :: [String] -> ValveMap
parseInputLines = foldl parseInputLineFold Data.Map.empty

parseInputLineFold :: ValveMap -> String -> ValveMap
parseInputLineFold valveMap inputLine = Data.Map.insert valveName valve valveMap
  where
    firstSplit = splitOn " has flow rate=" $ drop 6 inputLine
    valveName = head firstSplit
    possSecondSplit1 = splitOn "; tunnels lead to valves " (last firstSplit)
    possSecondSplit2 = splitOn "; tunnel leads to valve " (last firstSplit)
    secondSplit = if length possSecondSplit1 == 2 then possSecondSplit1 else possSecondSplit2
    flowRate = UtilLib.readInt $ head secondSplit
    tunnelValves = splitOn ", " $ last secondSplit
    valve = Valve {flowRate, tunnelValves}

initialPosition :: Position
initialPosition =
  Position
    { valveNames = ValveNames ("AA", "AA"),
      openValves = Data.Set.empty,
      openedFlowRate = 0,
      totalFlow = 0,
      positionsSinceLastOpen = (Data.Set.singleton "AA", Data.Set.singleton "AA"),
      path = []
    }

initialSeenPositionMap :: SeenPositionMap
initialSeenPositionMap =
  Data.Map.singleton (ValveNames ("AA", "AA")) [(Data.Set.empty, 0)]

calcMaxFlowRate :: ValveMap -> Int
calcMaxFlowRate valveMap = sum $ map flowRate $ Data.Map.elems valveMap

calcMaxFlowRateReachable :: ValveMap -> Data.Set.Set ValveName -> Int
calcMaxFlowRateReachable valveMap seenValveNames =
  if foundMore
    then calcMaxFlowRateReachable valveMap newSeenValveNames
    else sum $ map (\seenValveName -> flowRate $ fromJust $ Data.Map.lookup seenValveName valveMap) $ Data.Set.elems seenValveNames
  where
    newSeenValveNames = Data.Set.fromList $ seenValveNameList ++ concatMap (\seenValveName -> tunnelValves $ fromJust $ Data.Map.lookup seenValveName valveMap) seenValveNameList
    foundMore = Data.Set.size seenValveNames < Data.Set.size newSeenValveNames
    seenValveNameList = Data.Set.elems seenValveNames

handleSteps :: ValveMap -> Int -> Int -> ([Position], SeenPositionMap)
handleSteps valveMap maxFlowRate stepCount = foldl (handleStepFold valveMap maxFlowRate) ([initialPosition], initialSeenPositionMap ) [1 .. stepCount]

handleStepFold :: ValveMap -> Int -> ([Position], SeenPositionMap) -> Int -> ([Position], SeenPositionMap)
handleStepFold valveMap _ ([], seenPositionMap) _ = ([], seenPositionMap)
handleStepFold valveMap maxFlowRate (position : positions, seenPositionMap) step =
  (newPositions ++ newerPositions, newerSeenPositionMap) 
  where
    (newPositions, newSeenPositionMap) = handleStep valveMap seenPositionMap maxFlowRate position
    (newerPositions, newerSeenPositionMap) = handleStepFold valveMap maxFlowRate (positions, newSeenPositionMap) step

handleStep :: ValveMap -> SeenPositionMap -> Int -> Position -> ([Position], SeenPositionMap)
handleStep valveMap seenPositionMap maxFlowRate position
  | openedFlowRate position < maxFlowRate = removeSeenPositions (openBothPosList ++ open1PosList ++ open2PosList ++ openNonePosList) seenPositionMap
  | otherwise = (doNothingPosList, seenPositionMap)
  where
    openBothPosList =
      [ position
          { openValves = Data.Set.insert posValveName2 $ Data.Set.insert posValveName1 posOpenValves,
            openedFlowRate = openedFlowRate position + posValveFlowRate1 + posValveFlowRate2,
            totalFlow = newTotalFlow,
            positionsSinceLastOpen = (Data.Set.singleton posValveName1, Data.Set.singleton posValveName2),
            path = (posValveName1, posValveName2, posOpenValves, openedFlowRate position, totalFlow position) : path position
          }
        | canOpen1 && canOpen2
      ]
    open1PosList =
      [ position
          { valveNames = ValveNames (posValveName1, newPosValveName2),
            openValves = Data.Set.insert posValveName1 posOpenValves,
            openedFlowRate = openedFlowRate position + posValveFlowRate1,
            totalFlow = newTotalFlow,
            positionsSinceLastOpen = (Data.Set.singleton posValveName1, Data.Set.insert newPosValveName2 $ snd posPositionsSinceLastOpen),
            path = (posValveName1, posValveName2, posOpenValves, openedFlowRate position, totalFlow position) : path position
          }
        | canOpen1,
          newPosValveName2 <- tunnelValves2,
          newPosValveName2 `notElem` snd posPositionsSinceLastOpen && newPosValveName2 /= posValveName1
      ]
    open2PosList =
      [ position
          { valveNames = ValveNames (newPosValveName1, posValveName2),
            openValves = Data.Set.insert posValveName2 posOpenValves,
            openedFlowRate = openedFlowRate position + posValveFlowRate2,
            totalFlow = newTotalFlow,
            positionsSinceLastOpen = (Data.Set.insert newPosValveName1 $ fst posPositionsSinceLastOpen, Data.Set.singleton posValveName2),
            path = (posValveName1, posValveName2, posOpenValves, openedFlowRate position, totalFlow position) : path position
          }
        | canOpen2,
          newPosValveName1 <- tunnelValves1,
          newPosValveName1 `notElem` fst posPositionsSinceLastOpen && newPosValveName1 /= posValveName2
      ]
    openNonePosList =
      [ position
          { valveNames = ValveNames (newPosValveName1, newPosValveName2),
            totalFlow = newTotalFlow,
            positionsSinceLastOpen = Data.Bifunctor.bimap (Data.Set.insert newPosValveName1) (Data.Set.insert newPosValveName2) posPositionsSinceLastOpen,
            path = (posValveName1, posValveName2, posOpenValves, openedFlowRate position, totalFlow position) : path position
          }
        | newPosValveName1 <- tunnelValves1,
          newPosValveName1 `notElem` fst posPositionsSinceLastOpen && newPosValveName1 /= posValveName2,
          newPosValveName2 <- tunnelValves2,
          newPosValveName2 `notElem` snd posPositionsSinceLastOpen && newPosValveName2 /= posValveName1,
          newPosValveName1 /= newPosValveName2
      ]
    doNothingPosList =
      [ position
          { totalFlow = newTotalFlow,
            path = (posValveName1, posValveName2, posOpenValves, openedFlowRate position, totalFlow position) : path position
          }
      ]
    canOpen1 = posValveName1 `notElem` openValves position && posValveFlowRate1 > 0
    canOpen2 = posValveName2 `notElem` openValves position && posValveFlowRate2 > 0
    posValveData1 = fromJust $ Data.Map.lookup posValveName1 valveMap
    posValveData2 = fromJust $ Data.Map.lookup posValveName2 valveMap
    posValveFlowRate1 = flowRate posValveData1
    posValveFlowRate2 = flowRate posValveData2
    tunnelValves1 = tunnelValves posValveData1
    tunnelValves2 = tunnelValves posValveData2
    ValveNames (posValveName1, posValveName2) = valveNames position
    posOpenValves = openValves position
    posPositionsSinceLastOpen = positionsSinceLastOpen position
    newTotalFlow = totalFlow position + openedFlowRate position

mergePositions :: [Position] -> [Position]
mergePositions [] = []
mergePositions (position : positions) = if betterPositionExists then mergedPositions else position : mergedPositions
  where
    betterPositionExists = case Data.List.find (\listPos -> openedFlowRate position < openedFlowRate listPos && totalFlow position < totalFlow listPos) positions of
      Just _ -> True
      _ -> False
    mergedPositions = mergePositions positions

removeSeenPositions :: [Position] -> SeenPositionMap -> ([Position], SeenPositionMap)
removeSeenPositions [] seenPositionsMap = ([], seenPositionsMap)
removeSeenPositions (position : positions) seenPositionsMap = case Data.Map.lookup posValveNames seenPositionsMap of
  Just seenPositions ->
    if positionSeenAlready
      then removeSeenPositions positions seenPositionsMap
      else (position : newPositions, newSeenPositionMap)
    where
      positionSeenAlready = Data.List.any (\seenPosition -> posOpenValves `Data.Set.isSubsetOf` fst seenPosition && posTotalFlow <= snd seenPosition) seenPositions
      (newPositions, newSeenPositionMap) = removeSeenPositions positions updatedSeenPositionsMap
      updatedSeenPositionsMap = Data.Map.insert posValveNames newSeenPositions seenPositionsMap
        where
          newSeenPositions =
            (posOpenValves, posTotalFlow)
              : Data.List.filter (\seenPosition -> not $ fst seenPosition `Data.Set.isSubsetOf` posOpenValves && snd seenPosition <= posTotalFlow) seenPositions
  Nothing -> (position : newPositions, newSeenPositionMap)
    where
      (newPositions, newSeenPositionMap) = removeSeenPositions positions updatedSeenPositionsMap
      updatedSeenPositionsMap = Data.Map.insert posValveNames newSeenPositions seenPositionsMap
        where
          newSeenPositions = [(posOpenValves, posTotalFlow)]
  where
    posValveNames = valveNames position
    posOpenValves = openValves position
    posTotalFlow = totalFlow position