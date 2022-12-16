{-# LANGUAGE NamedFieldPuns #-}

module Task1Lib (taskFunc) where

import Control.Monad.State (MonadState (get, put), State)
import Data.Char (ord)
import Data.List (elemIndex, insert, intercalate, nub, stripPrefix)
import Data.List.Split (chunk, splitOn)
import qualified Data.Map
import Data.Maybe (fromJust)
import qualified Data.Set (Set, delete, empty, fromList, union)
import UtilLib (countTrueGrid, every, readInt, replaceNth)

type ValveName = String
type ValveMap = Data.Map.Map String Valve

data Valve = Valve
  { flowRate :: Int,
    tunnelValves :: [String]
  }
  deriving (Eq, Show)

data Path = Path
  { position :: ValveName,
    openValves :: [ValveName],
    openedFlowRate :: Int,
    totalFlow:: Int,
    prevPosition :: ValveName,
    justOpenedValve :: Bool
  }
  deriving (Eq, Show)

taskFunc :: [String] -> IO ()
taskFunc inputLines = do
  putStrLn "Input data:"
  let inputData = parseInputLines inputLines
  print inputData

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
