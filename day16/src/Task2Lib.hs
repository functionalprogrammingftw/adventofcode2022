{-# LANGUAGE NamedFieldPuns #-}

module Task2Lib (taskFunc) where

import Control.Monad.State (MonadState (get, put), State)
import Data.Char (ord)
import Data.List (elemIndex, insert, intercalate, nub, stripPrefix)
import Data.List.Split (chunk, splitOn)
import qualified Data.Map (Map, empty, insert, lookup)
import Data.Maybe (fromJust)
import qualified Data.Set (Set, delete, empty, fromList, insert, isSubsetOf, singleton, union)
import UtilLib (countTrueGrid, every, readInt, replaceNth)

type ValveName = String

type ValveMap = Data.Map.Map String Valve

data Valve = Valve
  { flowRate :: Int,
    tunnelValves :: [String]
  }
  deriving (Eq, Show)

data Position = Position
  { valveNames :: (ValveName, ValveName),
    openValves :: Data.Set.Set ValveName,
    openedFlowRate :: Int,
    totalFlow :: Int,
    positionsSinceLastOpen :: Data.Set.Set ValveName
  }
  deriving (Eq, Show)

taskFunc :: [String] -> IO ()
taskFunc inputLines = do
  putStrLn "Valve map:"
  let valveMap = parseInputLines inputLines
  print valveMap
  putStrLn "Position length:"
  let positions = handleSteps valveMap 3
  print $ length positions
  putStrLn "Opened flow rate:"
  let maxPressure = maximum $ map openedFlowRate positions
  print maxPressure
  putStrLn "Maximum pressure:"
  let maxPressure = maximum $ map totalFlow positions
  print maxPressure
--   putStrLn "Test:"
--   print $ filter (\position -> openedFlowRate position >= 81) positions

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
    { valveNames = ("AA", "AA"),
      openValves = Data.Set.empty,
      openedFlowRate = 0,
      totalFlow = 0,
      positionsSinceLastOpen = Data.Set.singleton "AA"
    }

handleSteps :: ValveMap -> Int -> [Position]
handleSteps valveMap stepCount = foldl (handleStepFold valveMap) [initialPosition] [1 .. stepCount]

handleStepFold :: ValveMap -> [Position] -> Int -> [Position]
handleStepFold valveMap [] _ = []
handleStepFold valveMap (position : positions) step = mergePositions (handleStepTest valveMap position) (handleStepFold valveMap positions step)

handleStepTest :: ValveMap -> Position -> [Position]
handleStepTest valveMap position = openBothPosList ++ open1PosList ++ open2PosList ++ openNonePosList
  where
    openBothPosList =
      [ position
          { openValves = Data.Set.insert posValveName2 $ Data.Set.insert posValveName1 posOpenValves,
            openedFlowRate = openedFlowRate position + posValveFlowRate1 + posValveFlowRate2,
            totalFlow = newTotalFlow,
            positionsSinceLastOpen = Data.Set.singleton posValveName1
          }
        | canOpen1 && canOpen2
      ]
    open1PosList =
      [ position
          { valveNames = (posValveName1, newPosValveName2),
            openValves = Data.Set.insert posValveName1 posOpenValves,
            openedFlowRate = openedFlowRate position + posValveFlowRate1,
            totalFlow = newTotalFlow,
            positionsSinceLastOpen = Data.Set.fromList [posValveName1, posValveName2, newPosValveName2]
          }
        | canOpen1,
          newPosValveName2 <- tunnelValves2,
          newPosValveName2 `notElem` posPositionsSinceLastOpen && newPosValveName2 /= posValveName1
      ]
    open2PosList =
      [ position
          { valveNames = (newPosValveName1, posValveName2),
            openValves = Data.Set.insert posValveName2 posOpenValves,
            openedFlowRate = openedFlowRate position + posValveFlowRate2,
            totalFlow = newTotalFlow,
            positionsSinceLastOpen = Data.Set.fromList [posValveName1, newPosValveName1, posValveName2]
          }
        | canOpen2,
          newPosValveName1 <- tunnelValves1,
          newPosValveName1 `notElem` posPositionsSinceLastOpen && newPosValveName1 /= posValveName2
      ]
    openNonePosList =
      [ position
          { valveNames = (newPosValveName1, newPosValveName2),
            totalFlow = newTotalFlow,
            positionsSinceLastOpen = Data.Set.insert newPosValveName2 $ Data.Set.insert newPosValveName1 posPositionsSinceLastOpen
          }
        | newPosValveName1 <- tunnelValves1,
          newPosValveName1 `notElem` posPositionsSinceLastOpen && newPosValveName1 /= posValveName2,
          newPosValveName2 <- tunnelValves2,
          newPosValveName2 `notElem` posPositionsSinceLastOpen && newPosValveName2 /= posValveName1,
          newPosValveName1 /= newPosValveName2
      ]
    canOpen1 = posValveName1 `notElem` openValves position && posValveFlowRate1 > 0
    canOpen2 = posValveName2 `notElem` openValves position && posValveFlowRate2 > 0
    posValveData1 = fromJust $ Data.Map.lookup posValveName1 valveMap
    posValveData2 = fromJust $ Data.Map.lookup posValveName2 valveMap
    posValveFlowRate1 = flowRate posValveData1
    posValveFlowRate2 = flowRate posValveData2
    tunnelValves1 = tunnelValves posValveData1
    tunnelValves2 = tunnelValves posValveData2
    (posValveName1, posValveName2) = valveNames position
    posOpenValves = openValves position
    posPositionsSinceLastOpen = positionsSinceLastOpen position
    newTotalFlow = totalFlow position + openedFlowRate position

mergePositions :: [Position] -> [Position] -> [Position]
mergePositions [] positions = positions
mergePositions (toMergePosition : toMergePositions) positions = if betterPositionExists then mergedPositions else toMergePosition : mergedPositions
  where
    betterPositionExists =
      not $
        null
          [ betterPosition
            | betterPosition <- positions,
              valveNamesSame betterPosition toMergePosition
                && openedFlowRate toMergePosition < openedFlowRate betterPosition
                && totalFlow toMergePosition < totalFlow betterPosition
          ]
    mergedPositions = mergePositions toMergePositions positions

valveNamesSame :: Position -> Position -> Bool
valveNamesSame position1 position2 =
  valveName11 == valveName21 && valveName12 == valveName22
    || valveName11 == valveName22 && valveName12 == valveName21
  where
    (valveName11, valveName12) = valveNames position1
    (valveName21, valveName22) = valveNames position2