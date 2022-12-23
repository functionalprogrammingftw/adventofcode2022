{-# LANGUAGE NamedFieldPuns #-}

module Task1Lib (taskFunc) where

import Control.Monad.State (MonadState (get, put), State)
import Data.Char (ord)
import qualified Data.HashSet (HashSet, delete, empty, fromList, insert, isSubsetOf, member, singleton, union)
import Data.List (elemIndex, insert, intercalate, nub, stripPrefix)
import Data.List.Split (chunk, split, splitOn)
import qualified Data.Map (Map, empty, insert, lookup)
import Data.Maybe (catMaybes, fromJust)
import UtilLib (countTrueGrid, every, readInt, replaceNth)

taskFunc :: [String] -> IO ()
taskFunc inputLines = do
  putStrLn "Blueprints:"
  let blueprints = parseInputLines inputLines
  print blueprints
  -- putStrLn "Blueprint:"
  -- let blueprint = blueprints !! 3
  -- print blueprint
  -- let (inventories, buyOrders) = calcBlueprintInventories 22 blueprint
  -- putStrLn "Blueprint 4 inventories after minutes:"
  -- print inventories
  -- putStrLn "Blueprint 4 buy orders length after minutes:"
  -- print $ length buyOrders
  -- putStrLn "Blueprint 4 inventory count after minutes:"
  -- print $ length inventories
  -- putStrLn "Geode robot inventory count:"
  -- print $ length $ filter (\i -> geodeRobots i > 0) inventories
  -- putStrLn "Max geode robot inventory count:"
  -- print $ maximum $ map geodes inventories
  qualityLevel <- calcBlueprintsQualityLevel 24 blueprints
  putStrLn "Sum quality levels:"
  print qualityLevel

parseInputLines :: [String] -> [Blueprint]
parseInputLines = map parseInputLine

parseInputLine :: String -> Blueprint
parseInputLine str = Blueprint {blueprintId, oreRobotPrice, clayRobotPrice, obsidianRobotPrice, geodeRobotPrice}
  where
    [startStr, endStr] = splitOn ": " str
    blueprintId = UtilLib.readInt $ drop 10 startStr
    [oreRobotPriceStr, clayRobotPriceStr, obsidianRobotPriceStr, geodeRobotPriceStr] = splitOn ". " endStr
    oreRobotPrice =
      RobotPrice
        { orePrice = UtilLib.readInt $ head $ splitOn " " $ drop 21 oreRobotPriceStr,
          clayPrice = 0,
          obsidianPrice = 0
        }
    clayRobotPrice =
      RobotPrice
        { orePrice = UtilLib.readInt $ head $ splitOn " " $ drop 22 clayRobotPriceStr,
          clayPrice = 0,
          obsidianPrice = 0
        }
    obsidianRobotPrice =
      RobotPrice
        { orePrice = UtilLib.readInt $ head $ splitOn " " $ drop 26 obsidianRobotPriceStr,
          clayPrice = UtilLib.readInt $ head $ splitOn " " $ last $ splitOn " ore and " obsidianRobotPriceStr,
          obsidianPrice = 0
        }
    geodeRobotPrice =
      RobotPrice
        { orePrice = UtilLib.readInt $ head $ splitOn " " $ drop 23 geodeRobotPriceStr,
          clayPrice = 0,
          obsidianPrice = UtilLib.readInt $ head $ splitOn " " $ last $ splitOn " ore and " geodeRobotPriceStr
        }

initialInventory :: Inventory
initialInventory =
  Inventory
    { oreRobots = 1,
      clayRobots = 0,
      obsidianRobots = 0,
      geodeRobots = 0,
      ore = 0,
      clay = 0,
      obsidian = 0,
      geodes = 0,
      buyOrder = ""
    }

calcBlueprintsQualityLevel :: Int -> [Blueprint] -> IO Int
calcBlueprintsQualityLevel minutes [] = return 0
calcBlueprintsQualityLevel minutes (blueprint : blueprints) = do
  level <- calcBlueprintQualityLevel minutes blueprint
  levels <- calcBlueprintsQualityLevel minutes blueprints
  return $ level + levels

calcBlueprintQualityLevel :: Int -> Blueprint -> IO Int
calcBlueprintQualityLevel minutes blueprint = do
  let maximumGeodes = maximum (map geodes $ fst $ calcBlueprintInventories minutes blueprint)
  let level = blueprintId blueprint * maximumGeodes
  putStrLn ("Blueprint " ++ show (blueprintId blueprint) ++ " quality level:")
  putStrLn (show (blueprintId blueprint) ++ " * " ++ show maximumGeodes ++ " = " ++ show level)
  return level

calcBlueprintInventories :: Int -> Blueprint -> ([Inventory], BuyOrders)
calcBlueprintInventories minutes blueprint = foldl (handleMinuteInventoriesAndPrune blueprint) ([initialInventory], Data.HashSet.empty) [1 .. minutes]

handleMinuteInventoriesAndPrune :: Blueprint -> ([Inventory], BuyOrders) -> Int -> ([Inventory], BuyOrders)
handleMinuteInventoriesAndPrune blueprint inventoriesAndBuyOrders minuteNo = (newInventories, newBuyOrders)
  where
    (inventories, buyOrders) = handleMinuteInventories blueprint inventoriesAndBuyOrders minuteNo
    newInventories = prune inventories
    newBuyOrders
      | length inventories /= length newInventories = Data.HashSet.fromList $ map buyOrder newInventories
      | otherwise = buyOrders

prune inventories
  | maxClayRobots > maxClayRobotDiff || maxObsidianRobots > maxObsidianRobotDiff || maxGeodeRobots > maxGeodeRobotDiff =
      filter
        (\i -> diffClayRobotsFilter i && diffObsidianRobotsFilter i && diffGeodeRobotsFilter i)
        inventories
  | otherwise = inventories
  where
    maxClayRobotDiff = 4
    maxObsidianRobotDiff = 3
    maxGeodeRobotDiff = 1
    maxClayRobots = maximum $ map clayRobots inventories
    maxObsidianRobots = maximum $ map obsidianRobots inventories
    maxGeodeRobots = maximum $ map geodeRobots inventories
    diffClayRobotsFilter inventory =
      clayRobots inventory >= maxClayRobots - maxClayRobotDiff
        || obsidianRobots inventory >= maxObsidianRobots - maxObsidianRobotDiff
        || geodeRobots inventory >= maxGeodeRobots - maxGeodeRobotDiff
    diffObsidianRobotsFilter inventory =
      obsidianRobots inventory >= maxObsidianRobots - maxObsidianRobotDiff
        || geodeRobots inventory >= maxGeodeRobots - maxGeodeRobotDiff
    diffGeodeRobotsFilter inventory =
      geodeRobots inventory >= maxGeodeRobots - maxGeodeRobotDiff

handleMinuteInventories :: Blueprint -> ([Inventory], BuyOrders) -> Int -> ([Inventory], BuyOrders)
handleMinuteInventories blueprint ([], buyOrders) minuteNo = ([], buyOrders)
handleMinuteInventories blueprint (inventory : inventories, buyOrders) minuteNo =
  (newInventories ++ newestInventories, newestBuyOrders)
  where
    (newInventories, newBuyOrders) = handleMinuteInventory blueprint inventory buyOrders
    (newestInventories, newestBuyOrders) = handleMinuteInventories blueprint (inventories, newBuyOrders) minuteNo

handleMinuteInventory :: Blueprint -> Inventory -> BuyOrders -> ([Inventory], BuyOrders)
handleMinuteInventory blueprint inventory buyOrders = (map (produce inventory) boughtRobotsInventories, newBuyOrders)
  where
    boughtRobotsInventories =
      inventory
        : catMaybes
          [ buyRobot 'O' (oreRobotPrice blueprint) buyOrders oreRobotIncrementer inventory,
            buyRobot 'C' (clayRobotPrice blueprint) buyOrders clayRobotIncrementer inventory,
            buyRobot 'B' (obsidianRobotPrice blueprint) buyOrders obsidianRobotIncrementer inventory,
            buyRobot 'G' (geodeRobotPrice blueprint) buyOrders geodeRobotIncrementer inventory
          ]
    newBuyOrders = Data.HashSet.union buyOrders $ Data.HashSet.fromList $ map buyOrder boughtRobotsInventories

buyRobot :: RobotType -> RobotPrice -> BuyOrders -> (Inventory -> Inventory) -> Inventory -> Maybe Inventory
buyRobot robotType robotPrice buyOrders robotIncrementer inventory =
  if ore newInventory >= 0 && clay newInventory >= 0 && obsidian newInventory >= 0 && not (buyOrder newInventory `Data.HashSet.member` buyOrders)
    then Just newInventory
    else Nothing
  where
    newInventory =
      robotIncrementer
        inventory
          { ore = ore inventory - orePrice robotPrice,
            clay = clay inventory - clayPrice robotPrice,
            obsidian = obsidian inventory - obsidianPrice robotPrice,
            buyOrder = robotType : buyOrder inventory
          }

produce :: Inventory -> Inventory -> Inventory
produce originalInventory inventory =
  inventory
    { ore = ore inventory + oreRobots originalInventory,
      clay = clay inventory + clayRobots originalInventory,
      obsidian = obsidian inventory + obsidianRobots originalInventory,
      geodes = geodes inventory + geodeRobots originalInventory
    }

oreRobotIncrementer :: Inventory -> Inventory
oreRobotIncrementer inventory = inventory {oreRobots = oreRobots inventory + 1}

clayRobotIncrementer :: Inventory -> Inventory
clayRobotIncrementer inventory = inventory {clayRobots = clayRobots inventory + 1}

obsidianRobotIncrementer :: Inventory -> Inventory
obsidianRobotIncrementer inventory = inventory {obsidianRobots = obsidianRobots inventory + 1}

geodeRobotIncrementer :: Inventory -> Inventory
geodeRobotIncrementer inventory = inventory {geodeRobots = geodeRobots inventory + 1}

-- Model
type BuyOrder = [RobotType]

type BuyOrders = Data.HashSet.HashSet BuyOrder

type RobotType = Char

data RobotPrice = RobotPrice
  { orePrice :: Int,
    clayPrice :: Int,
    obsidianPrice :: Int
  }
  deriving (Show, Eq)

data Blueprint = Blueprint
  { blueprintId :: Int,
    oreRobotPrice :: RobotPrice,
    clayRobotPrice :: RobotPrice,
    obsidianRobotPrice :: RobotPrice,
    geodeRobotPrice :: RobotPrice
  }
  deriving (Show, Eq)

data Inventory = Inventory
  { oreRobots :: Int,
    clayRobots :: Int,
    obsidianRobots :: Int,
    geodeRobots :: Int,
    ore :: Int,
    clay :: Int,
    obsidian :: Int,
    geodes :: Int,
    buyOrder :: BuyOrder
  }
  deriving (Show, Eq)
