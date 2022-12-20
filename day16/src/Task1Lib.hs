{-# LANGUAGE NamedFieldPuns #-}

module Task1Lib (taskFunc) where

import Control.Monad.State (MonadState (get, put), State)
import Data.Char (ord)
import Data.List (elemIndex, insert, intercalate, nub, stripPrefix)
import Data.List.Split (chunk, splitOn)
import qualified Data.Map (Map, empty, insert, lookup)
import Data.Maybe (fromJust)
import qualified Data.Set (Set, delete, empty, fromList, insert, singleton, union, isSubsetOf)
import UtilLib (countTrueGrid, every, readInt, replaceNth)

type ValveName = String

type ValveMap = Data.Map.Map String Valve

data Valve = Valve
  { flowRate :: Int,
    tunnelValves :: [String]
  }
  deriving (Eq, Show)

data Position = Position
  { valveName :: ValveName,
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
  let positions = handleSteps valveMap 30
  print $ length positions
  putStrLn "Opened flow rate:"
  let maxOpenedFlowRate = maximum $ map openedFlowRate positions
  print maxOpenedFlowRate
  putStrLn "Maximum pressure:"
  let maxPressure = maximum $ map totalFlow positions
  print maxPressure

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

handleSteps :: ValveMap -> Int -> [Position]
handleSteps valveMap stepCount = foldl (handleStep valveMap) [initialPosition] [1 .. stepCount]
  where
    initialPosition =
      Position
        { valveName = "AA",
          openValves = Data.Set.empty,
          openedFlowRate = 0,
          totalFlow = 0,
          positionsSinceLastOpen = Data.Set.singleton "AA"
        }

handleStep :: ValveMap -> [Position] -> Int -> [Position]
handleStep valveMap [] _ = []
handleStep valveMap (position : positions) step = mergePositions (openPosList ++ newPosList) (handleStep valveMap positions step)
  where
    openPosList =
      [ position
          { openValves = Data.Set.insert posValveName posOpenValves,
            openedFlowRate = openedFlowRate position + posValveFlowRate,
            totalFlow = totalFlow position + openedFlowRate position,
            positionsSinceLastOpen = Data.Set.singleton posValveName
          }
        | canOpen
      ]
    newPosList =
      [ position
          { valveName = newPosValveName,
            totalFlow = totalFlow position + openedFlowRate position,
            positionsSinceLastOpen = Data.Set.insert newPosValveName posPositionsSinceLastOpen
          }
        | newPosValveName <- tunnelValves posValveData,
          newPosValveName `notElem` posPositionsSinceLastOpen
      ]
    canOpen = posValveName `notElem` openValves position && posValveFlowRate > 0
    posValveData = fromJust $ Data.Map.lookup posValveName valveMap
    posValveFlowRate = flowRate $ posValveData
    posValveName = valveName position
    posOpenValves = openValves position
    posPositionsSinceLastOpen = positionsSinceLastOpen position

mergePositions :: [Position] -> [Position] -> [Position]
mergePositions [] positions = positions
mergePositions (toMergePosition : toMergePositions) positions = if betterPositionExists then mergedPositions else toMergePosition:mergedPositions
  where
    betterPositionExists =
      not $
        null
          [ betterPosition
            | betterPosition <- positions,
              valveName betterPosition == valveName toMergePosition
                && openedFlowRate toMergePosition < openedFlowRate betterPosition
                && totalFlow toMergePosition < totalFlow betterPosition
          ]
    mergedPositions = mergePositions toMergePositions positions
